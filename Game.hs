-- Módulo que implementa el bucle principal del juego.
module Game (playGame, playGameWithCallback, generateRandomObstacles, generateRandomObstaclesWithRobots) where

import Graphics.Gloss hiding (Vector, Point)
import Graphics.Gloss.Interface.Pure.Game hiding (Vector, Point)
import Graphics.Gloss.Interface.IO.Game (playIO)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Control.Concurrent (threadDelay)

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)

import Geometry
import Robot (Robot(..), Turret(..), MovementAction(..), MemoryValue(..), isRobotAlive, createBasicRobot, updateRobotVelocity)
import qualified Robot as R
import Entities (Projectile(..), GameEntity(..), ID, Explosion(..), updateExplosion, isExplosionActive, isExplosionDamaging, createExplosion, Obstacle(..), ObstacleType(..), ObstacleShape(..))
import qualified Entities as E
import qualified AI
import GameState
import Data.Default (def)
import Rendering (drawGame)
import RandomUtils (generatePositionFromSeed, generateSafeRobotPosition, generateNonOverlappingObstacles)
import Collisions (checkCollisions, checkCollision, detectRobotObstacleCollisions, detectProjectileObstacleCollisions, willCollideNextFrame, RobotProjectileCollisionEvent, RobotRobotCollisionEvent)
import Geometry (add2D, prodByScalar, translateVertices)
import UIButton (UIButton(..))

-- Regenera los robots con nuevas posiciones aleatorias basadas en una semilla
-- Esta función se llama cuando el jugador presiona 'R' para resetear el juego
--
-- Parámetros:
--   currentState:  Estado actual del juego (contiene el tiempo transcurrido que se usa como semilla)
--   initialState:  Estado inicial limpio del juego (sin proyectiles, explosiones, etc.)
--
-- Retorna: Un nuevo estado de juego con robots reposicionados aleatoriamente
regenerateRobotsWithRandomPositions :: GameState -> GameState -> GameState
regenerateRobotsWithRandomPositions currentState initialState =
  initialState
    { gameRobots = newRobots
    , gameObstacles = newObstacles
    , gameSimulationSpeed = gameSimulationSpeed currentState
    , gameDebugInfo = gameDebugInfo currentState
    , gameWindowSize = gameWindowSize currentState
    , gameSeed = gameSeed currentState
  }
  where
    seedBase = gameSeed currentState
    stageSize' = gameStageSize initialState
    bounds = (fst stageSize' / 2, snd stageSize' / 2)

    originalRobots = Map.elems (gameRobots initialState)
    robotInfos = [(robotID r, robotBehavior r) | r <- originalRobots]

    -- 1) Reposicionar robots de forma determinista (sin depender de obstáculos)
    newRobots = Map.fromList
      [ (rid, createBasicRobot (generateSafeRobotPosition bounds seedBase rid []) behavior rid)
        | (rid, behavior) <- robotInfos
      ]

    -- 2) Generar obstáculos evitando solapar robots ni entre ellos
    newObstacles = Map.fromList [ (obstacleID o, o) | o <- generateNonOverlappingObstacles stageSize' (realToFrac seedBase) (Map.elems newRobots) ]

playGame :: GameState -> IO ()
playGame initialState = playGameWithCallback initialState (\_ -> return Nothing)

-- Versión de play que permite un callback IO cuando termina cada partida.
-- El callback recibe el GameState final de la partida y debe devolver:
--   Just nextState -> continuar con el siguiente torneo usando nextState
--   Nothing        -> detener la secuencia (se quedará en pantalla pausada)
playGameWithCallback :: GameState -> (GameState -> IO (Maybe GameState)) -> IO ()
playGameWithCallback initialState callback = do
  calledRef <- newIORef False
  let fps = 60
      backgroundColor = white
      window = InWindow "Juego de torneos" (gameWindowSize initialState) (100,100)

      drawIO s = return (drawGame s)

      handleIO ev s = return (handleEvents [tryHandleResizing, tryHandleMouse, tryHandleKeys] ev s)

      preUpdateIO :: Float -> GameState -> IO GameState
      preUpdateIO dt preRealTimeGS = do
        let gs = preRealTimeGS { gameSeed = gameSeed preRealTimeGS + realToFrac dt * 12347 }
            keys = gameKeysPressed gs
            preUpdatedState
              | Set.member (Char 'd') keys = gs { gameDebugInfo = not (gameDebugInfo gs), gameKeysPressed = Set.delete (Char 'd') keys }
              | Set.member (Char '1') keys = gs { gameSimulationSpeed = 0.1 }
              | Set.member (Char '2') keys = gs { gameSimulationSpeed = 0.25 }
              | Set.member (Char '3') keys = gs { gameSimulationSpeed = 0.5 }
              | Set.member (Char '4') keys = gs { gameSimulationSpeed = 0.75 }
              | Set.member (Char '5') keys = gs { gameSimulationSpeed = 1.0 }
              | Set.member (Char '6') keys = gs { gameSimulationSpeed = 1.25 }
              | Set.member (Char '7') keys = gs { gameSimulationSpeed = 1.5 }
              | Set.member (Char '8') keys = gs { gameSimulationSpeed = 2.0 }
              | Set.member (Char '9') keys = gs { gameSimulationSpeed = 2.5 }
              | Set.member (Char '0') keys = gs { gameSimulationSpeed = 3.0 }
              | otherwise = gs

        let nextState
              | gameIsInMenu gs = preUpdatedState
              | Set.member (Char 'r') keys = regenerateRobotsWithRandomPositions gs initialState
              | Set.member (SpecialKey KeySpace) keys && gamePaused gs = updateGame dt (preUpdatedState { gamePaused = not (gamePaused gs), gameKeysPressed = Set.delete (SpecialKey KeySpace) keys })
              | Set.member (SpecialKey KeySpace) keys && not (gamePaused gs) = preUpdatedState { gamePaused = not (gamePaused gs), gameKeysPressed = Set.delete (SpecialKey KeySpace) keys }
              | gamePaused gs = preUpdatedState
              | otherwise = updateGame dt preUpdatedState

        -- Si la partida está pausada por haber quedado un robot, llamar al callback UNA VEZ
        let robotsLeft = length (Map.elems (gameRobots nextState))
        alreadyCalled <- readIORef calledRef
        if robotsLeft <= 1 && gamePaused nextState && not alreadyCalled
          then do
            writeIORef calledRef True
            -- llamar callback para permitir logging/estadísticas
            mNext <- callback nextState
            -- Si callback devuelve un estado explícito, usarlo (y continuar)
            case mNext of
              Just ns -> do
                writeIORef calledRef False
                return (ns { gamePaused = False })
              Nothing -> do
                -- Si estamos en modo torneo y quedan partidas, crear la siguiente partida tras 2s
                if gameTournamentActive nextState && gameTournamentRemaining nextState > 1
                  then do
                    -- Esperar 2 segundos
                    threadDelay (2 * 1000000)
                    -- Construir siguiente GameState a partir de la configuración de torneo en el estado actual
                    let cfgs = gameTournamentConfigs nextState
                        stage = gameStageSize nextState
                        seedBase' = gameTournamentSeed nextState + 1
                        -- Usar la semilla incrementada en el estado actual
                        currWithSeed = nextState { gameSeed = seedBase' }
                        -- Plantilla inicial para regenerar usando la lógica de Game
                        initialTemplate = def { gameStageSize = stage
                                              , gameBotConfigs = cfgs
                                              , gameWindowSize = gameWindowSize nextState
                                              , gameImages = gameImages nextState
                                              , gameIsInMenu = False
                                              , gameRobots = Map.fromList [ (rid, createBasicRobot (0,0) beh rid) | (rid, beh) <- cfgs ]
                                              , gameTotalRobotCount = length cfgs
                                              }
                        regenerated = regenerateRobotsWithRandomPositions currWithSeed initialTemplate
                        ids = map fst cfgs
                        newStats = Map.fromList [ (i, emptyRobotStats) | i <- ids ]
                        newRemaining = gameTournamentRemaining nextState - 1
                        newState = regenerated { gameStats = newStats
                                               , gamePaused = False
                                               , gameTournamentRemaining = newRemaining
                                               , gameTournamentSeed = seedBase'
                                               }
                    -- Permitir que el callback pueda dispararse de nuevo en la próxima partida
                    writeIORef calledRef False
                    return newState
                  else return nextState
          else return nextState

  playIO window backgroundColor fps initialState drawIO handleIO preUpdateIO

-- Generación determinista de obstáculos (verificando que no se generen sobre robots)
generateRandomObstacles :: Size -> Float -> [Obstacle]
generateRandomObstacles stageSize seed = generateRandomObstaclesWithRobots stageSize seed []

generateRandomObstaclesWithRobots :: Size -> Float -> [(Float, Float)] -> [Obstacle]
generateRandomObstaclesWithRobots (w,h) seed robotPositions = generateObstaclesSequentially [] ids
  where
    bounds = (w/2, h/2)
    ids = [1001..]
    minDistanceToRobot = 12.0 :: Float  -- Distancia mínima entre obstáculo y robot
    minDistanceBetweenObstacles = 15.0 :: Float  -- Distancia mínima entre obstáculos
    maxObstacles = 6  -- Máximo número de obstáculos

    -- Distribución de tipos de obstáculos (33.33% cada uno - probabilidad igual)
    pickType rid = let p = frac (sin (seed*0.73 + fromIntegral rid*12.3) * 43758.5453)
                   in if p < 0.3333
                      then Solid    -- 33.33%: No pasa nada (solo impide el paso)
                      else if p < 0.6666
                           then Hazard  -- 33.33%: Hace daño al tocarlos
                           else Bomb    -- 33.33%: Cuenta atrás y explosión
    frac x = x - fromIntegral (floor x :: Int)

    distanceBetween (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

    -- Genera obstáculos secuencialmente verificando que no se solapen
    generateObstaclesSequentially :: [Obstacle] -> [Int] -> [Obstacle]
    generateObstaclesSequentially acc _ | length acc >= maxObstacles = acc
    generateObstaclesSequentially acc [] = acc
    generateObstaclesSequentially acc (rid:rids) =
      case tryMkObs 0 rid (map obstaclePosition acc) of
        Just obs -> generateObstaclesSequentially (obs : acc) rids
        Nothing -> generateObstaclesSequentially acc rids  -- Si falla, intenta con el siguiente ID

    -- Intenta generar un obstáculo, reintentando si está muy cerca de un robot u otro obstáculo
    tryMkObs :: Int -> Int -> [(Float, Float)] -> Maybe Obstacle
    tryMkObs attempt rid existingObstaclePositions
      | attempt >= 100 = Nothing  -- Fallback: si después de 100 intentos no encuentra posición, no genera el obstáculo
      | otherwise =
          -- Generador 2D hash-based independiente para romper patrones en diagonal
          -- Dos hashes distintos para X e Y en [0,1)
          let frac' x = x - fromIntegral (floor x :: Int)
              u = realToFrac $ frac' (sin (seed*0.873 + fromIntegral rid*12.9898 + fromIntegral attempt*78.233) * 43758.5453)
              v = realToFrac $ frac' (sin (seed*1.327 + fromIntegral rid*4.1234  + fromIntegral attempt*93.733) * 15731.7431)
              (bx,by) = bounds
              pos = ((u*2-1)*bx, (v*2-1)*by)
              tooCloseToRobot = any (\robotPos -> distanceBetween pos robotPos < minDistanceToRobot) robotPositions
              tooCloseToObstacle = any (\obstaclePos -> distanceBetween pos obstaclePos < minDistanceBetweenObstacles) existingObstaclePositions
          in if tooCloseToRobot || tooCloseToObstacle
             then tryMkObs (attempt + 1) rid existingObstaclePositions
             else Just (mkObs rid pos)

    mkObs rid pos =
      let t = pickType rid
          (shape, sz, col) = case t of
            Solid  -> (E.Square, (10,10), greyN 0.5)              -- GRIS - Solo impide el paso
            Hazard -> (E.Circle, (8,8), makeColor 1 0 0 0.9)      -- ROJO - Hace daño constante
            Bomb   -> (E.Square, (7,7), makeColor 1 1 0 0.9)      -- AMARILLO - Cuenta atrás y explosión
            Special-> (E.Square, (8,8), makeColor 0.5 0.5 0.5 0.5) -- (No debería generarse)
          localVerts = case shape of
            E.Square -> let (sx, sy) = sz in [(-sx/2,-sy/2),(sx/2,-sy/2),(sx/2,sy/2),(-sx/2,sy/2)]
            E.Circle -> circleApproxVerts (fst sz / 2) 16
            E.Polygon pts -> pts
          worldVerts = map (add2D pos) localVerts
      in Obs { obstacleID = rid
             , obstacleType = t
             , obstacleShape = shape
             , obstaclePosition = pos
             , obstacleVertices = worldVerts
             , obstacleSize = sz
             , obstacleOrientation = 0
             , obstacleHealth = 100
             , obstacleTimer = Nothing
             , obstacleColor = col
             }

    -- Utilidades de geometría para formas
    circleApproxVerts r n = [ (r * cos (theta i), r * sin (theta i)) | i <- [0..n-1] ]
      where theta i = 2*pi*fromIntegral i / fromIntegral n
    regularPolygonVerts n r = [ (r * cos (theta i), r * sin (theta i)) | i <- [0..n-1] ]
      where theta i = 2*pi*fromIntegral i / fromIntegral n

-- ==========================
-- Manejo de eventos
-- ==========================
type MaybeEventHandler s = Event -> s -> Maybe s

handleEvents :: [MaybeEventHandler GameState] -> Event -> GameState -> GameState
handleEvents handlers event gs = case [ h event gs | h <- handlers, h event gs /= Nothing ] of
  (Just s:_) -> s
  _          -> gs

tryHandleKeys :: Event -> GameState -> Maybe GameState
tryHandleKeys event st = case event of
  EventKey k Down _ _ -> Just st { gameKeysPressed = Set.insert k (gameKeysPressed st) }
  EventKey k Up   _ _ -> Just st { gameKeysPressed = Set.delete k (gameKeysPressed st) }
  _                   -> Nothing

tryHandleResizing :: Event -> GameState -> Maybe GameState
tryHandleResizing event st = case event of
  EventResize size' -> Just st { gameWindowSize = size' }
  _                 -> Nothing

tryHandleMouse :: Event -> GameState -> Maybe GameState
tryHandleMouse event st = case event of
  EventKey (MouseButton LeftButton) Down _ (mx,my) -> Just (applyButtons (mx,my) st)
  _                                               -> Nothing
  where
    applyButtons :: (Float,Float) -> GameState -> GameState
    applyButtons (mx,my) gs
      | not (gameIsInMenu gs) = gs
      | otherwise = foldl applyIfInside gs (gameButtons gs)
      where
        (winW, winH) = (fromIntegral *** fromIntegral) (gameWindowSize gs)
        (***) f g (a,b) = (f a, g b)
        applyIfInside acc btn =
          let (bx',by') = buttonPosition btn
              (bsx',bsy') = buttonSize btn
              bx = winW/2 * bx'; by = winH/2 * by'
              bw = winW/2 * bsx'; bh = winH/2 * bsy'
          in if mx >= bx - bw/2 && mx <= bx + bw/2 && my >= by - bh/2 && my <= by + bh/2
               then buttonHandler btn acc
               else acc

-- ==========================
-- Lógica principal de actualización
-- ==========================
updateGame :: Float -> GameState -> GameState
updateGame dt oldState = finalState
  where
    -- actualizar estadísticas de tiempo vivo para cada robot
    updateTimeAlive :: Map.Map ID RobotStats -> [Robot] -> Float -> Map.Map ID RobotStats
    updateTimeAlive stats robots dtInc = foldl upd stats robots
      where
        upd m r = if isRobotAlive r
                  then Map.insertWith combine (robotID r) (emptyRobotStats { timeAlive = dtInc }) m
                  else m
        combine new old = old { timeAlive = timeAlive old + timeAlive new }

    deltaTime = dt * gameSimulationSpeed oldState
    stageSize'@(width,height) = gameStageSize oldState

    -- Actualizar tiempo, frame y cooldown de colisiones
    newTime = gameTime oldState + deltaTime
    newFrame = gameFrame oldState + 1
    newCollisionCooldown = max 0 (gameCollisionCooldown oldState - deltaTime)

    -- Inicializar obstáculos al empezar partida si aún no hay
    baseObstacles =
      if Map.null (gameObstacles oldState) && not (gameIsInMenu oldState)
        then Map.fromList [ (obstacleID o, o) | o <- generateRandomObstacles stageSize' (realToFrac (gameSeed oldState)) ]
        else gameObstacles oldState

    -- Actualizar explosiones vivas
    updatedExplosions = Map.filter isExplosionActive $ fmap (\e -> updateExplosion e deltaTime) (gameExplosions oldState)

    -- Aplicar física simple: mover y corregir a límites
    applyPhysics :: GameEntity a => a -> a
    applyPhysics entity = clampToBounds stageSize' (updatePosition entity deltaTime)

    clampToBounds :: GameEntity a => Size -> a -> a
    clampToBounds (w,h) e = finalE
      where
        (x,y) = position e
        (vx, vy) = velocity e
        minX = -w/2; maxX = w/2; minY = -h/2; maxY = h/2
        margin = 2.0  -- Margen para detección suave

        -- Verificar si está cerca o fuera de los límites
        nearLeft = x < minX + margin
        nearRight = x > maxX - margin
        nearTop = y > maxY - margin
        nearBottom = y < minY + margin

        -- Clampear posición si está fuera
        cx = max minX (min maxX x)
        cy = max minY (min maxY y)

        -- Rebote suave: invertir solo la componente de velocidad que va hacia el límite
        nvx = if (nearLeft && vx < 0) || (nearRight && vx > 0) then -vx * 0.6 else vx
        nvy = if (nearBottom && vy < 0) || (nearTop && vy > 0) then -vy * 0.6 else vy
        
        -- Solo aplicar cambios si realmente está fuera o muy cerca
        needsCorrection = x /= cx || y /= cy || nearLeft || nearRight || nearTop || nearBottom

        finalE = if needsCorrection
                 then let t = (cx - x, cy - y)
                          newPos = (cx, cy)
                          newVerts = translateVertices (vertices e) t
                      in setVertices (setPosition (setVelocity e (nvx, nvy)) newPos) newVerts
                 else e

    withinBounds :: Size -> (Float,Float) -> Bool
    withinBounds (w,h) (x,y) = x > -w/2 && x < w/2 && y > -h/2 && y < h/2

    movedProjectiles = fmap (\p -> updatePosition p deltaTime) (gameProjectiles oldState)
    keptProjectiles = Map.filter (\p -> withinBounds stageSize' (position p)) movedProjectiles

    -- incrementar tiempo vivo en estadísticas
    statsAfterTime = updateTimeAlive (gameStats oldState) (Map.elems $ gameRobots oldState) deltaTime

    phisicsState = oldState
      { gameProjectiles = keptProjectiles
      , gameRobots      = fmap applyPhysics (gameRobots oldState)
      , gameExplosions  = updatedExplosions
      , gameStats = statsAfterTime
      , gameObstacles   = baseObstacles
      , gameTime        = newTime
      , gameFrame       = newFrame
      , gameCollisionCooldown = newCollisionCooldown
      }

    -- Ya no necesitamos handleMapEdge complejo, el rebote suave en clampToBounds es suficiente
    edgeState = phisicsState

    -- Predicción de colisiones Robot–Obstáculo (pre-IA)
    robotObstaclePredictions =
      [ (r, o)
      | r <- Map.elems (gameRobots edgeState)
      , o <- Map.elems (gameObstacles edgeState)
      , willCollideNextFrame r o 0.3
      ]

    predictedState = foldl
      (\acc (r, _o) -> acc { gameRobots = Map.adjust (AI.avoidObstacleSmartImmediate acc) (robotID r) (gameRobots acc) })
      edgeState
      robotObstaclePredictions

    -- Colisiones básicas
    (robotProjectileCollisions, robotRobotCollisions) = checkCollisions (Map.elems $ gameRobots predictedState) (Map.elems $ gameProjectiles predictedState)
    robotObstacleCollisions = detectRobotObstacleCollisions (Map.elems $ gameRobots predictedState) (Map.elems $ gameObstacles predictedState)
    projectileObstacleCollisions = detectProjectileObstacleCollisions (Map.elems $ gameProjectiles predictedState) (Map.elems $ gameObstacles predictedState)

    -- Explosiones existentes afectan a entidades
    maxTime = 1 :: Float
    maxRadiusProj = 5 :: Float
    maxRadiusMine = 30 :: Float
    explDamage = 30 :: Float

    applyExplosions :: [Explosion] -> GameState -> GameState
    applyExplosions [] gs = gs
    applyExplosions (e:es) gs = applyExplosions es (applyExplosion e gs)
      where
        applyExplosion :: Explosion -> GameState -> GameState
        applyExplosion ex state =
          applyExplosionToProjectiles (Map.elems $ gameProjectiles state) $
          applyExplosionToRobots (Map.elems $ gameRobots state) state
          where
            checkExplosionGameEntity :: GameEntity a => a -> Bool
            checkExplosionGameEntity a = Collisions.checkCollision (explosionVertices ex) (vertices a)

            applyExplosionToRobots :: [Robot] -> GameState -> GameState
            applyExplosionToRobots [] gs' = gs'
            applyExplosionToRobots (r:rs) gs'
              | not (checkExplosionGameEntity r) || not (isExplosionDamaging ex) = applyExplosionToRobots rs gs'
              | isRobotAlive updatedRobot' = applyExplosionToRobots rs (gs' { gameRobots = Map.insert (robotID updatedRobot') updatedRobot' (gameRobots gs') })
              | otherwise = applyExplosionToRobots rs (gs' { gameRobots = Map.delete (robotID updatedRobot') (gameRobots gs') })
              where
                -- empuje ligero desde el centro de la explosión
                (rx, ry) = position r
                (exx, exy) = explosionPosition ex
                vx = rx - exx; vy = ry - exy
                m = sqrt (vx*vx + vy*vy)
                dir = if m < 1e-6 then (0,0) else (vx/m, vy/m)
                push = prodByScalar (1.5 * deltaTime) dir
                newPos = add2D (position r) push
                newVerts = translateVertices (vertices r) push
                updatedRobot' = setVertices (setPosition (r { robotEnergy = robotEnergy r - deltaTime * explosionDamage ex }) newPos) newVerts

            applyExplosionToProjectiles :: [Projectile] -> GameState -> GameState
            applyExplosionToProjectiles [] gs' = gs'
            applyExplosionToProjectiles (p:ps) gs'
              | checkExplosionGameEntity p && isExplosionDamaging ex = applyExplosionToProjectiles ps updatedGS
              | otherwise = applyExplosionToProjectiles ps gs'
              where
                totalExplosionCount = gameTotalExplosionCount gs'
                updatedGS = gs'
                  { gameProjectiles = Map.delete (projectileID p) (gameProjectiles gs')
                  , gameExplosions  = Map.insert newID newExpl (gameExplosions gs')
                  , gameTotalExplosionCount = totalExplosionCount + 1
                  }
                newID = totalExplosionCount
                newExpl = createExplosion (position p) maxRadiusProj explDamage maxTime newID

    -- Proyectil–Robot
    applyRobotProjectileCollisions :: [RobotProjectileCollisionEvent] -> GameState -> GameState
    applyRobotProjectileCollisions [] gs = gs
    applyRobotProjectileCollisions ((p, r):colls) gs = applyRobotProjectileCollisions colls updatedGS
      where
        -- actualizar contador de impactos para el robot objetivo
        statsMap = gameStats gs
        prevStats = Map.findWithDefault emptyRobotStats (robotID r) statsMap
        incStats = prevStats { hitsReceived = hitsReceived prevStats + 1 }
        statsWithHit = Map.insert (robotID r) incStats statsMap

        updatedGS = if projectileOwnerID p == robotID r
          then gs
          else gs { gameRobots = updatedRobots
                  , gameProjectiles = Map.delete (projectileID p) (gameProjectiles gs)
                  , gameExplosions = Map.insert newID newExpl (gameExplosions gs)
                  , gameTotalExplosionCount = totalExplosionCount + 1
                  , gameStats = statsWithHit
                  }
        totalExplosionCount = gameTotalExplosionCount gs
        robotsMap = gameRobots gs
        updatedRobots
          | isRobotAlive updatedR = Map.insert (robotID r) updatedR robotsMap
          | otherwise             = Map.delete (robotID r) robotsMap
          where
            updatedR = r { robotEnergy = robotEnergy r - projectileDamage p }
        newID = totalExplosionCount
        newExpl = createExplosion (position p) maxRadiusProj explDamage maxTime newID

    -- Robot–Robot (solo daño cruzado sencillo)
    applyRobotRobotCollisions :: [RobotRobotCollisionEvent] -> GameState -> GameState
    applyRobotRobotCollisions [] gs = gs
    applyRobotRobotCollisions ((r1, r2):colls) gs = applyRobotRobotCollisions colls updatedGS
      where
        robotRobotDamage = 10 :: Float  -- Aumentado de 5 a 10 DPS
        adjustRobot :: Robot -> Map.Map ID Robot -> Map.Map ID Robot
        adjustRobot r robotsMap
          | isRobotAlive updatedR = Map.insert (robotID r) updatedR robotsMap
          | otherwise             = Map.delete (robotID r) robotsMap
          where
            updatedR = r { robotEnergy = robotEnergy r - robotRobotDamage * deltaTime }
        updatedGS = gs { gameRobots = adjustRobot r1 (adjustRobot r2 (gameRobots gs)) }

    collisionState =
        applyRobotRobotCollisions robotRobotCollisions $
        applyRobotProjectileCollisions robotProjectileCollisions $
      applyExplosions (Map.elems $ gameExplosions predictedState) predictedState

    -- Obstáculos - Recalcular colisiones con el estado actualizado
    -- ORDEN IMPORTANTE: bombas primero (para que se activen antes de que el robot retroceda), luego hazard, luego solid
    handleObstacleEffects :: GameState -> GameState
    handleObstacleEffects gs =
      let debugObstacles = if gameDebugInfo gs && (gameFrame gs `mod` 60 == 0)  -- Cada 60 frames (1 segundo)
                           then trace ("=== OBSTÁCULOS: " ++ show [(obstacleID o, obstacleType o) | o <- Map.elems (gameObstacles gs)])
                           else id
      in debugObstacles $ solidStep . hazardStep . detonateStep $ gs
      where
        hazardDamage = 15 :: Float  -- Daño instantáneo por colisión (más que robot-robot que es 10 DPS)

        -- Robot sólido: detener, retroceder POCO y girar 90° (sin empujar/atravesar)
        solidStep :: GameState -> GameState
        solidStep s = foldl applySolid s solidCollisions
          where
            -- Recalcular colisiones PARA SOLID con el estado actual
            solidCollisions = detectRobotObstacleCollisions (Map.elems $ gameRobots s) (Map.elems $ gameObstacles s)
            applySolid acc (r, o, _push)
              | obstacleType o /= Solid = acc
              | otherwise =
                  let stopped = setVelocity r (0,0)
                      backed  = updateRobotVelocity stopped (R.MoveBackward 0.5)  -- Reducido de 1.5 a 0.5
                      rotated = updateRobotVelocity backed (R.Rotate (pi/2))
                  in acc { gameRobots = Map.insert (robotID r) rotated (gameRobots acc) }

        hazardStep :: GameState -> GameState
        hazardStep s =
          let allCollisions = detectRobotObstacleCollisions (Map.elems $ gameRobots s) (Map.elems $ gameObstacles s)
              hazardOnly = filter (\(_, o, _) -> obstacleType o == Hazard) allCollisions
              debugAllColl = if gameDebugInfo s && not (null allCollisions)
                             then trace ("=== TODAS LAS COLISIONES: " ++ show [(robotID r, obstacleID o, obstacleType o) | (r, o, _) <- allCollisions])
                             else id
              debugHazards = if gameDebugInfo s && not (null hazardOnly)
                             then trace ("=== COLISIONES HAZARD: " ++ show [(robotID r, obstacleID o) | (r, o, _) <- hazardOnly])
                             else id
          in debugAllColl $ debugHazards $ foldl applyHazard s allCollisions
          where
            applyHazard acc (r, o, _)
              | obstacleType o /= Hazard = acc
              | otherwise =
                  let debugHazardMsg = if gameDebugInfo acc
                                       then trace ("Robot " ++ show (robotID r) ++ " chocó con HAZARD " ++ show (obstacleID o) ++ " - Aplicando " ++ show hazardDamage ++ " de daño!")
                                       else id
                  in debugHazardMsg $ case Map.lookup (robotID r) (gameRobots acc) of
                    Just currentRobot ->
                      let energiaBefore = robotEnergy currentRobot
                          updatedRobot = currentRobot { robotEnergy = energiaBefore - hazardDamage }
                          energiaAfter = robotEnergy updatedRobot
                          debugEnergy = if gameDebugInfo acc
                                        then trace ("Energía: " ++ show energiaBefore ++ " -> " ++ show energiaAfter)
                                        else id
                          -- Crear efecto de colisión IGUAL que cuando impacta un proyectil
                          collisionEffect = createExplosion (obstaclePosition o) maxRadiusProj explDamage maxTime (gameTotalExplosionCount acc)
                          newExplosions = Map.insert (gameTotalExplosionCount acc) collisionEffect (gameExplosions acc)
                      in debugEnergy $ if isRobotAlive updatedRobot
                         then acc { gameRobots = Map.insert (robotID r) updatedRobot (gameRobots acc)
                                  , gameExplosions = newExplosions
                                  , gameTotalExplosionCount = gameTotalExplosionCount acc + 1
                                  }
                         else let debugDeath = if gameDebugInfo acc
                                               then trace ("Robot " ++ show (robotID r) ++ " MURIÓ por Hazard!")
                                               else id
                              in debugDeath $ acc { gameRobots = Map.delete (robotID r) (gameRobots acc)
                                                  , gameExplosions = newExplosions
                                                  , gameTotalExplosionCount = gameTotalExplosionCount acc + 1
                                                  }  -- Eliminar si muere
                    Nothing -> acc

        detonateStep :: GameState -> GameState
        detonateStep s0 = s3
          where
            -- Recalcular colisiones PARA BOMBAS con el estado actual (después de hazardStep)
            bombCollisions = detectRobotObstacleCollisions (Map.elems $ gameRobots s0) (Map.elems $ gameObstacles s0)
            -- Activar bombas tocadas (solo si no están ya activadas)
            s1 = foldl activate s0 bombCollisions
            activate acc (r, o, _)
              | obstacleType o /= Bomb = acc
              | otherwise =
                  let debugMsg = if gameDebugInfo acc
                                 then trace ("Robot " ++ show (robotID r) ++ " tocó bomba " ++ show (obstacleID o))
                                 else id
                  in debugMsg $ case Map.lookup (obstacleID o) (gameObstacles acc) of
                    Just bomb ->
                      case obstacleTimer bomb of
                        Nothing ->
                          let activatedBomb = bomb { obstacleTimer = Just 2.0 }
                              debugActivate = if gameDebugInfo acc
                                              then trace ("¡Bomba " ++ show (obstacleID o) ++ " ACTIVADA con timer 2.0!")
                                              else id
                          in debugActivate $ acc { gameObstacles = Map.insert (obstacleID o) activatedBomb (gameObstacles acc) }
                        Just t ->
                          let debugAlready = if gameDebugInfo acc
                                             then trace ("Bomba " ++ show (obstacleID o) ++ " ya está activada, timer: " ++ show t)
                                             else id
                          in debugAlready acc  -- Ya está activada, no hacer nada
                    Nothing -> acc

            -- Actualizar timers de TODAS las bombas activadas
            (s2, toExplode) = Map.foldlWithKey advance (s1, []) (gameObstacles s1)
            advance (acc, ex) oid ob
              | obstacleType ob /= Bomb = (acc, ex)  -- Solo procesar bombas
              | otherwise = case obstacleTimer ob of
                  Just t
                    | t - deltaTime <= 0 ->
                        -- Bomba explota: eliminarla y añadir a lista de explosiones
                        (acc { gameObstacles = Map.delete oid (gameObstacles acc) }, (oid, ob):ex)
                    | otherwise ->
                        -- Decrementar timer
                        (acc { gameObstacles = Map.insert oid (ob { obstacleTimer = Just (t - deltaTime) }) (gameObstacles acc) }, ex)
                  Nothing -> (acc, ex)  -- Bomba no activada todavía

            -- Crear explosiones para bombas que explotaron
            s3 = foldl mkExplosion s2 toExplode
            mkExplosion acc (_, ob) =
              let totalE = gameTotalExplosionCount acc
                  eid = totalE
                  bombDamage = 80 :: Float  -- Aumentado de 60 a 80
                  expl = createExplosion (obstaclePosition ob) maxRadiusMine bombDamage 0.8 eid
              in acc { gameExplosions = Map.insert eid expl (gameExplosions acc)
                     , gameTotalExplosionCount = totalE + 1 }

        normalizeVec :: (Float, Float) -> (Float, Float)
        normalizeVec (x,y) = let m = sqrt (x*x + y*y) in if m < 1e-6 then (0,0) else (x/m, y/m)

    handleProjectileObstacle :: GameState -> GameState
    handleProjectileObstacle gs = foldl step gs projectileObstacleCollisions
      where
        step acc (p, _) = acc { gameProjectiles = Map.delete (projectileID p) (gameProjectiles acc) }

    afterObstacles = handleProjectileObstacle (handleObstacleEffects collisionState)

    -- IA
    aiResults = fmap (\r -> AI.updateRobotAI r collisionState deltaTime) (gameRobots collisionState)
    updatedRobots = fmap AI.updatedRobot aiResults
    spawnedProjectiles = concatMap AI.newProjectiles aiResults

    insertSpawnedProjectiles :: ID -> Map.Map ID Projectile -> [Projectile] -> Map.Map ID Projectile
    insertSpawnedProjectiles _ m [] = m
    insertSpawnedProjectiles nextID m (p:ps) = insertSpawnedProjectiles (nextID + 1) newM ps
        where
            newM = Map.insert nextID (p { projectileID = nextID }) m

    totalProjectileCount = gameTotalProjectileCount afterObstacles
    allProjectiles = insertSpawnedProjectiles totalProjectileCount (gameProjectiles afterObstacles) spawnedProjectiles

    finalState' = afterObstacles
      { gameRobots = updatedRobots
      , gameProjectiles = allProjectiles
      , gameTotalProjectileCount = totalProjectileCount + length spawnedProjectiles
      }

    finalState = case Map.elems (gameRobots finalState') of
      [ _lastR ] -> finalState' { gamePaused = True }
      _          -> finalState'
