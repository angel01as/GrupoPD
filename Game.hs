-- Módulo que implementa el bucle principal del juego.
module Game (playGame, playGameWithCallback) where

import Graphics.Gloss hiding (Vector, Point)
import Graphics.Gloss.Interface.Pure.Game hiding (Vector, Point)
import Graphics.Gloss.Interface.IO.Game (playIO)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import System.IO (openFile, hClose, IOMode(AppendMode))
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import Data.Char (isSpace)
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
import RandomUtils (generatePositionFromSeed, generateSafeRobotPosition, generateRandomObstacles, generateRandomObstaclesWithRobots, deriveSeed)
import Collisions (checkCollisions, checkCollision, detectRobotObstacleCollisions, detectProjectileObstacleCollisions, willCollideNextFrame, RobotProjectileCollisionEvent, RobotRobotCollisionEvent)
import UIButton (UIButton(..))
import TournamentStats (writeTournamentStats, writeAggregateStats)


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
    newObstacles = Map.fromList [ (obstacleID o, o) | o <- generateRandomObstaclesWithRobots stageSize' (realToFrac seedBase) (map robotPosition (Map.elems newRobots)) ]

-- Función para iniciar el torneo automáticamente después de 5 segundos de inactividad
autoStartTournament :: GameState -> GameState
autoStartTournament s =
  let -- Limpiar archivo de estadísticas anterior (sobrescribir)
      _ = unsafePerformIO $ writeFile "estadisticas.txt" ""

      cfgContent = unsafePerformIO (readFile "config.txt")
      -- Parser simple de config.txt
      parseConfigLocal content = (zip ids botList, (w,h), tournaments')
        where
          trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
          ls = map (takeWhile (/='\r')) $ lines content
          findKey k = case [ drop 1 (dropWhile (/=':') l) | l <- ls, take (length k) l == k ] of
                        (v:_) -> trim v
                        [] -> error $ "Missing config key: " ++ k
          botsStr = findKey "bots"
          splitByComma s' = case dropWhile (==',') s' of
            "" -> []
            s'' -> let (w', s''') = break (==',') s'' in w' : splitByComma s'''
          botList = map trim $ splitByComma botsStr
          ids = [1..length botList]
          stageStr = findKey "stage"
          (w,h) = case span (/='x') stageStr of
                    (a, 'x':b) -> (read a :: Float, read b :: Float)
                    _ -> error "stage must be WxH"
          tournaments' = read (findKey "tournaments") :: Int
      (cfgs, stageSize', tournaments) = parseConfigLocal cfgContent
      seedBase = gameSeed s
      bounds = (fst stageSize' / 2, snd stageSize' / 2)

      -- Generar robots con posiciones aleatorias
      minRobotDist = 15.0 :: Float
      distanceBetween (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

      generateRobotsSequentially :: [(Int, String)] -> [(Float, Float)] -> [(Int, Robot)]
      generateRobotsSequentially [] _ = []
      generateRobotsSequentially ((rid, behavior):rest) existingPositions =
        let safePos = findSafePosition rid 0 existingPositions
            robot = createBasicRobot safePos behavior rid
        in (rid, robot) : generateRobotsSequentially rest (safePos : existingPositions)

      findSafePosition :: Int -> Int -> [(Float, Float)] -> (Float, Float)
      findSafePosition rid attempt existingPos
        | attempt >= 500 = generatePositionFromSeed bounds (seedBase * fromIntegral rid) rid
        | otherwise =
            let candidatePos = generatePositionFromSeed bounds (seedBase + fromIntegral attempt * 0.137) rid
                tooCloseToRobot = any (\pos -> distanceBetween candidatePos pos < minRobotDist) existingPos
            in if tooCloseToRobot then findSafePosition rid (attempt + 1) existingPos else candidatePos

      newRobots = Map.fromList (generateRobotsSequentially cfgs [])
      robotPositions = [robotPosition r | (_, r) <- Map.toList newRobots]
      newObstaclesList = generateRandomObstaclesWithRobots stageSize' (realToFrac seedBase) robotPositions
      newObstacles = Map.fromList [ (obstacleID o, o) | o <- newObstaclesList ]
      ids = map fst cfgs
      newStats = Map.fromList [ (i, emptyRobotStats) | i <- ids ]

  in s { gameBotConfigs = cfgs
       , gameStageSize = stageSize'
       , gameTotalRobotCount = length cfgs
       , gameTournamentActive = True
       , gameTournamentRemaining = tournaments
       , gameTournamentSeed = gameSeed s
       , gameTournamentConfigs = cfgs
       , gameTournamentStatsFile = Just "estadisticas.txt"
       , gameTournamentCurrentIndex = 1
       , gameTournamentStatsHistory = []
       , gameIsInMenu = False
       , gameRobots = newRobots
       , gameObstacles = newObstacles
       , gameMenuTimer = 0
       , gameStats = newStats
       }

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
            -- Incrementar temporizador del menú si estamos en el menú
            updatedMenuTimer = if gameIsInMenu gs then gameMenuTimer gs + dt else 0
            preUpdatedState
              | Set.member (Char 'd') keys = gs { gameDebugInfo = not (gameDebugInfo gs), gameKeysPressed = Set.delete (Char 'd') keys, gameMenuTimer = 0 }
              | Set.member (Char '1') keys = gs { gameSimulationSpeed = 0.1, gameMenuTimer = 0 }
              | Set.member (Char '2') keys = gs { gameSimulationSpeed = 0.25, gameMenuTimer = 0 }
              | Set.member (Char '3') keys = gs { gameSimulationSpeed = 0.5, gameMenuTimer = 0 }
              | Set.member (Char '4') keys = gs { gameSimulationSpeed = 0.75, gameMenuTimer = 0 }
              | Set.member (Char '5') keys = gs { gameSimulationSpeed = 1.0, gameMenuTimer = 0 }
              | Set.member (Char '6') keys = gs { gameSimulationSpeed = 1.25, gameMenuTimer = 0 }
              | Set.member (Char '7') keys = gs { gameSimulationSpeed = 1.5, gameMenuTimer = 0 }
              | Set.member (Char '8') keys = gs { gameSimulationSpeed = 2.0, gameMenuTimer = 0 }
              | Set.member (Char '9') keys = gs { gameSimulationSpeed = 2.5, gameMenuTimer = 0 }
              | Set.member (Char '0') keys = gs { gameSimulationSpeed = 3.0, gameMenuTimer = 0 }
              | otherwise = gs { gameMenuTimer = updatedMenuTimer }

        let nextState
              | gameIsInMenu gs && gameMenuTimer preUpdatedState >= 5.0 = autoStartTournament preUpdatedState
              | gameIsInMenu gs = preUpdatedState
              | Set.member (Char 'r') keys = regenerateRobotsWithRandomPositions gs initialState
              | Set.member (SpecialKey KeySpace) keys && gamePaused gs = updateGame dt (preUpdatedState { gamePaused = not (gamePaused gs), gameKeysPressed = Set.delete (SpecialKey KeySpace) keys })
              | Set.member (SpecialKey KeySpace) keys && not (gamePaused gs) = preUpdatedState { gamePaused = not (gamePaused gs), gameKeysPressed = Set.delete (SpecialKey KeySpace) keys }
              | gamePaused gs = preUpdatedState
              | otherwise = updateGame dt preUpdatedState

        -- Si es un torneo y el archivo de estadísticas aún no se limpió, hacerlo ahora (en IO) y marcarlo
        clearedState <- if gameTournamentActive nextState && not (gameTournamentFileCleared nextState)
                        then case gameTournamentStatsFile nextState of
                               Just fp -> do
                                 writeFile fp ""
                                 return (nextState { gameTournamentFileCleared = True })
                               Nothing -> return nextState
                        else return nextState

        -- Si la partida está pausada por haber quedado un robot, llamar al callback UNA VEZ
        let robotsLeft = length (Map.elems (gameRobots clearedState))
        alreadyCalled <- readIORef calledRef
        if robotsLeft <= 1 && gamePaused clearedState && not alreadyCalled
          then do
            writeIORef calledRef True

            -- Si estamos en modo torneo, guardar estadísticas
            when (gameTournamentActive clearedState) $ do
              case gameTournamentStatsFile clearedState of
                Just filePath -> do
                  h <- openFile filePath AppendMode
                  writeTournamentStats h (gameTournamentCurrentIndex clearedState) clearedState
                  hClose h
                Nothing -> return ()

            -- llamar callback para permitir logging/estadísticas
            mNext <- callback nextState
            -- Si callback devuelve un estado explícito, usarlo (y continuar)
            case mNext of
              Just ns -> do
                writeIORef calledRef False
                return (ns { gamePaused = False })
              Nothing -> do
                -- Si estamos en modo torneo y quedan partidas, crear la siguiente partida tras 2s
                if gameTournamentActive clearedState && gameTournamentRemaining clearedState > 1
                  then do
                    -- Esperar 2 segundos
                    threadDelay (2 * 1000000)
                    -- Construir siguiente GameState a partir de la configuración de torneo en el estado actual
                    let cfgs = gameTournamentConfigs clearedState
                        stage = gameStageSize clearedState
                        -- Derivar semilla con más entropía para la siguiente partida
                        seedBase' = deriveSeed (gameTournamentSeed clearedState) (gameTournamentCurrentIndex clearedState)
                        -- Usar la semilla incrementada en el estado actual
                        currWithSeed = clearedState { gameSeed = seedBase' }
                        -- Plantilla inicial para regenerar usando la lógica de Game
                        initialTemplate = def { gameStageSize = stage
                                              , gameBotConfigs = cfgs
                                              , gameWindowSize = gameWindowSize clearedState
                                              , gameImages = gameImages clearedState
                                              , gameIsInMenu = False
                                              , gameRobots = Map.fromList [ (rid, createBasicRobot (0,0) beh rid) | (rid, beh) <- cfgs ]
                                              , gameTotalRobotCount = length cfgs
                                              }
                        regenerated = regenerateRobotsWithRandomPositions currWithSeed initialTemplate
                        ids = map fst cfgs
                        newStats = Map.fromList [ (i, emptyRobotStats) | i <- ids ]
                        newRemaining = gameTournamentRemaining clearedState - 1
                        -- Agregar estadísticas actuales al historial
                        newHistory = gameStats clearedState : gameTournamentStatsHistory clearedState
                        newState = regenerated { gameStats = newStats
                                               , gamePaused = False
                                               , gameTournamentActive = True
                                               , gameTournamentRemaining = newRemaining
                                               , gameTournamentSeed = seedBase'
                                               , gameTournamentConfigs = cfgs
                                               , gameTournamentStatsFile = gameTournamentStatsFile clearedState
                                               , gameTournamentFileCleared = gameTournamentFileCleared clearedState
                                               , gameTournamentCurrentIndex = gameTournamentCurrentIndex clearedState + 1
                                               , gameTournamentStatsHistory = newHistory
                                               }
                    -- Permitir que el callback pueda dispararse de nuevo en la próxima partida
                    writeIORef calledRef False
                    return newState
                  else do
                    -- Último torneo terminado - escribir estadísticas agregadas
                    when (gameTournamentActive clearedState) $ do
                      let finalHistory = gameStats clearedState : gameTournamentStatsHistory clearedState
                      case gameTournamentStatsFile clearedState of
                        Just filePath -> do
                          h <- openFile filePath AppendMode
                          writeAggregateStats h (reverse finalHistory)
                          hClose h
                        Nothing -> return ()
                    return clearedState
          else return nextState

  playIO window backgroundColor fps initialState drawIO handleIO preUpdateIO



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
  EventKey (MouseButton LeftButton) Down _ (mx,my) -> Just (applyButtons (mx,my) (st { gameMenuTimer = 0 }))
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
        nvx = if (nearLeft && vx < 0) || (nearRight && vx > 0) then-vx * 0.6 else vx
        nvy = if (nearBottom && vy < 0) || (nearTop && vy > 0) then-vy * 0.6 else vy

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
        -- actualizar estadísticas
        statsMap = gameStats gs

        -- Actualizar hitsReceived para el robot que recibe el impacto
        targetStats = Map.findWithDefault emptyRobotStats (robotID r) statsMap
        updatedTargetStats = targetStats { hitsReceived = hitsReceived targetStats + 1 }
        statsWithHitReceived = Map.insert (robotID r) updatedTargetStats statsMap

        -- Actualizar hitsLanded para el robot que disparó
        shooterStats = Map.findWithDefault emptyRobotStats (projectileOwnerID p) statsWithHitReceived
        updatedShooterStats = shooterStats { hitsLanded = hitsLanded shooterStats + 1 }
        statsWithHitLanded = Map.insert (projectileOwnerID p) updatedShooterStats statsWithHitReceived

        -- Si el robot muere, incrementar kills del atacante
        updatedR = r { robotEnergy = robotEnergy r - projectileDamage p }
        robotDied = not (isRobotAlive updatedR)
        finalStats = if robotDied
                     then let shooterStatsWithKill = Map.findWithDefault emptyRobotStats (projectileOwnerID p) statsWithHitLanded
                              updatedShooterWithKill = shooterStatsWithKill { kills = kills shooterStatsWithKill + 1 }
                          in Map.insert (projectileOwnerID p) updatedShooterWithKill statsWithHitLanded
                     else statsWithHitLanded

        updatedGS = if projectileOwnerID p == robotID r
          then gs
          else gs { gameRobots = updatedRobots
                  , gameProjectiles = Map.delete (projectileID p) (gameProjectiles gs)
                  , gameExplosions = Map.insert newID newExpl (gameExplosions gs)
                  , gameTotalExplosionCount = totalExplosionCount + 1
                  , gameStats = finalStats
                  }
        totalExplosionCount = gameTotalExplosionCount gs
        robotsMap = gameRobots gs
        updatedRobots
          | isRobotAlive updatedR = Map.insert (robotID r) updatedR robotsMap
          | otherwise             = Map.delete (robotID r) robotsMap
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

    -- Actualizar estadísticas de disparos
    updateShotsFired :: Map.Map ID RobotStats -> [Projectile] -> Map.Map ID RobotStats
    updateShotsFired stats [] = stats
    updateShotsFired stats (p:ps) =
      let ownerID = projectileOwnerID p
          ownerStats = Map.findWithDefault emptyRobotStats ownerID stats
          updatedOwnerStats = ownerStats { shotsFired = shotsFired ownerStats + 1 }
          updatedStats = Map.insert ownerID updatedOwnerStats stats
      in updateShotsFired updatedStats ps

    statsWithShots = updateShotsFired (gameStats afterObstacles) spawnedProjectiles

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
      , gameStats = statsWithShots
      }

    finalState = case Map.elems (gameRobots finalState') of
      [ _lastR ] -> finalState' { gamePaused = True }
      _          -> finalState'
