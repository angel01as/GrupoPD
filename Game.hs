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
import Data.Maybe (mapMaybe, isJust)
import Data.Char (isSpace)
import Data.List (sortOn)
import Debug.Trace (trace)

import Geometry
import Robot (Robot(..), Turret(..), MovementAction(..), MemoryValue(..), isRobotAlive, createBasicRobot, updateRobotVelocity)
import qualified Robot as R
import Entities (Projectile(..), GameEntity(..), ID, Explosion(..), updateExplosion, isExplosionActive, isExplosionDamaging, createExplosion, Obstacle(..), ObstacleType(..), ObstacleShape(..), Missile(..))
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
    , gameMissiles = Map.empty
    , gameHazardAnimations = Map.empty
    , gameAirplane = Nothing
    , gameAirplaneCooldown = 0
    , gameAirplanePassCount = 0
    , gameTotalMissileCount = 0
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
      , gameMissiles = Map.empty
      , gameHazardAnimations = Map.empty
      , gameAirplane = Nothing
      , gameAirplaneCooldown = 0
      , gameAirplanePassCount = 0
      , gameTotalMissileCount = 0
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

        -- Determinar el siguiente estado base: o bien procesando la cuenta atrás, o aplicando entradas/actualización
        baseState <- if gameTournamentActive gs && gameTournamentCountdown gs > 0 then do
          let t' = max 0 (gameTournamentCountdown gs - realToFrac dt)
          if t' > 0 then
            return (gs { gameTournamentCountdown = t' })
          else do
            -- Cuenta atrás terminó: iniciar siguiente torneo si queda
            if gameTournamentRemaining gs > 1 then do
              let cfgs = gameTournamentConfigs gs
                  stage = gameStageSize gs
                  seedBase' = deriveSeed (gameTournamentSeed gs) (gameTournamentCurrentIndex gs)
                  currWithSeed = gs { gameSeed = seedBase' }
                  initialTemplate = def { gameStageSize = stage
                                        , gameBotConfigs = cfgs
                                        , gameWindowSize = gameWindowSize gs
                                        , gameImages = gameImages gs
                                        , gameIsInMenu = False
                                        , gameRobots = Map.fromList [ (rid, createBasicRobot (0,0) beh rid) | (rid, beh) <- cfgs ]
                                        , gameTotalRobotCount = length cfgs }
                  regenerated = regenerateRobotsWithRandomPositions currWithSeed initialTemplate
                  ids = map fst cfgs
                  newStats = Map.fromList [ (i, emptyRobotStats) | i <- ids ]
                  newRemaining = gameTournamentRemaining gs - 1
                  newHistory = gameStats gs : gameTournamentStatsHistory gs
                  newState = regenerated { gameStats = newStats
                                         , gamePaused = False
                                         , gameTournamentActive = True
                                         , gameTournamentRemaining = newRemaining
                                         , gameTournamentSeed = seedBase'
                                         , gameTournamentConfigs = cfgs
                                         , gameTournamentStatsFile = gameTournamentStatsFile gs
                                         , gameTournamentFileCleared = gameTournamentFileCleared gs
                                         , gameTournamentCurrentIndex = gameTournamentCurrentIndex gs + 1
                                         , gameTournamentStatsHistory = newHistory
                                         , gameTournamentCountdown = 0 }
              writeIORef calledRef False
              return newState
            else return (gs { gameTournamentCountdown = 0 })
        else do
          let updatedMenuTimer = if gameIsInMenu gs then gameMenuTimer gs + dt else 0
          let preUpdatedState
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
          return nextState

        -- Si es un torneo y el archivo de estadísticas aún no se limpió, hacerlo ahora (en IO) y marcarlo
        clearedState <- if gameTournamentActive baseState && not (gameTournamentFileCleared baseState)
                        then case gameTournamentStatsFile baseState of
                               Just fp -> do
                                 writeFile fp ""
                                 return (baseState { gameTournamentFileCleared = True })
                               Nothing -> return baseState
                        else return baseState

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
            mNext <- callback baseState
            -- Si callback devuelve un estado explícito, usarlo (y continuar)
            case mNext of
              Just ns -> do
                writeIORef calledRef False
                return (ns { gamePaused = False })
              Nothing -> do
                -- Si estamos en modo torneo y quedan partidas, crear la siguiente partida tras 2s
                if gameTournamentActive clearedState && gameTournamentRemaining clearedState > 1
                  then do
                    -- Iniciar cuenta atrás de 3 segundos para el siguiente torneo
                    return (clearedState { gameTournamentCountdown = 3.0 })
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
          else return baseState

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

    airSupportState = updateAirSupport deltaTime edgeState
    missileUpdatedState = updateMissiles deltaTime airSupportState
    hazardAnimState = updateHazardAnimations deltaTime missileUpdatedState

    -- Predicción de colisiones Robot–Obstáculo (pre-IA)
    robotObstaclePredictions =
      [ (r, o)
      | r <- Map.elems (gameRobots hazardAnimState)
      , o <- Map.elems (gameObstacles hazardAnimState)
      , willCollideNextFrame r o 0.3
      ]

    predictedState = foldl
      (\acc (r, _o) -> acc { gameRobots = Map.adjust (AI.avoidObstacleSmartImmediate acc) (robotID r) (gameRobots acc) })
      hazardAnimState
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
        hazardDamage = 30 :: Float  -- Daño instantáneo por colisión (más que robot-robot que es 10 DPS)

        -- Robot sólido: resolver penetración y deslizar suavemente alrededor del obstáculo
        solidStep :: GameState -> GameState
        solidStep s = foldl applySolid s solidCollisions
          where
            -- Recalcular colisiones PARA SOLID con el estado actual
            solidCollisions = detectRobotObstacleCollisions (Map.elems $ gameRobots s) (Map.elems $ gameObstacles s)
            applySolid acc (r, o, push)
              | obstacleType o /= Solid = acc
              | otherwise =
                  case Map.lookup (robotID r) (gameRobots acc) of
                    Nothing -> acc
                    Just currentRobot ->
                      let resolvedRobot = resolvePenetration currentRobot push
                          normal = normalizeVec push
                          tangent = ensureNonZero (perp normal)
                          glideDir = if dotVec (velocity resolvedRobot) tangent >= 0 then tangent else negateVec tangent
                          desiredDir = normalizeVec (add2D (prodByScalar 0.8 glideDir) (prodByScalar 0.2 (angleFactor (orientation resolvedRobot))))
                          desiredAngle = atan2 (snd desiredDir) (fst desiredDir)
                          turnStep = clampRange (normalizeAngleLocal (desiredAngle - orientation resolvedRobot)) (-pi/18, pi/18)
                          orientedRobot = updateRobotVelocity (setVelocity resolvedRobot (prodByScalar 0.25 (velocity resolvedRobot))) (R.Rotate turnStep)
                          blendedVelocity = add2D (prodByScalar 1.1 desiredDir) (prodByScalar 0.3 (velocity orientedRobot))
                          finalRobot = setVelocity orientedRobot blendedVelocity
                      in acc { gameRobots = Map.insert (robotID finalRobot) finalRobot (gameRobots acc) }

            resolvePenetration :: Robot -> (Float, Float) -> Robot
            resolvePenetration robot pushVec =
              let translation = prodByScalar 1.05 pushVec
                  newPos = add2D (position robot) translation
                  newVerts = translateVertices (vertices robot) translation
              in setVertices (setPosition robot newPos) newVerts

            normalizeVec :: (Float, Float) -> (Float, Float)
            normalizeVec (x, y)
              | mag < 1e-5 = (0,0)
              | otherwise = (x / mag, y / mag)
              where mag = sqrt (x*x + y*y)

            dotVec :: (Float, Float) -> (Float, Float) -> Float
            dotVec (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

            negateVec :: (Float, Float) -> (Float, Float)
            negateVec (x, y) = (-x, -y)

            ensureNonZero :: (Float, Float) -> (Float, Float)
            ensureNonZero v@(x, y)
              | abs x < 1e-5 && abs y < 1e-5 = (0,1)
              | otherwise = v

            normalizeAngleLocal :: Float -> Float
            normalizeAngleLocal a = atan2 (sin a) (cos a)

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
                  let accWithAnim = triggerHazardAnimation acc (obstacleID o)
                      debugHazardMsg = if gameDebugInfo accWithAnim
                                       then trace ("Robot " ++ show (robotID r) ++ " chocó con HAZARD " ++ show (obstacleID o) ++ " - Aplicando " ++ show hazardDamage ++ " de daño!")
                                       else id
                  in debugHazardMsg $ case Map.lookup (robotID r) (gameRobots accWithAnim) of
                    Just currentRobot ->
                      let energiaBefore = robotEnergy currentRobot
                          updatedRobot = currentRobot { robotEnergy = energiaBefore - hazardDamage }
                          energiaAfter = robotEnergy updatedRobot
                          debugEnergy = if gameDebugInfo accWithAnim
                                        then trace ("Energía: " ++ show energiaBefore ++ " -> " ++ show energiaAfter)
                                        else id
                      in debugEnergy $ if isRobotAlive updatedRobot
                         then accWithAnim { gameRobots = Map.insert (robotID r) updatedRobot (gameRobots accWithAnim) }
                         else let debugDeath = if gameDebugInfo accWithAnim
                                               then trace ("Robot " ++ show (robotID r) ++ " MURIÓ por Hazard!")
                                               else id
                              in debugDeath $ accWithAnim { gameRobots = Map.delete (robotID r) (gameRobots accWithAnim) }
                    Nothing -> accWithAnim

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
        -- Las bombas NO bloquean proyectiles ni se activan con ellos
        step acc (p, o)
          | obstacleType o == Bomb = acc    -- pasar de largo
          | otherwise =
              let accWithoutProjectile = acc { gameProjectiles = Map.delete (projectileID p) (gameProjectiles acc) }
              in case Map.lookup (obstacleID o) (gameObstacles accWithoutProjectile) of
                   Nothing -> accWithoutProjectile
                   Just currentObstacle ->
                     case obstacleType currentObstacle of
                       Solid ->
                         let newHits = obstacleHitCount currentObstacle + 1
                         in if newHits >= 4
                               then accWithoutProjectile { gameObstacles = Map.delete (obstacleID currentObstacle) (gameObstacles accWithoutProjectile) }
                               else let updatedObstacle = currentObstacle { obstacleHitCount = newHits }
                                    in accWithoutProjectile { gameObstacles = Map.insert (obstacleID currentObstacle) updatedObstacle (gameObstacles accWithoutProjectile) }
                       _ -> accWithoutProjectile

    missileFallSpeed = 45 :: Float
    missileExplosionRadius = 18 :: Float
    missileExplosionDamage = 70 :: Float
    missileExplosionDuration = 0.9 :: Float
    missileSpawnMargin = 8 :: Float
    hazardAnimationFrameDuration = 0.08 :: Float
    hazardAnimationCooldownDuration = 0.6 :: Float
    hazardAnimationFrameCount = 5 :: Int
    airplanePassInterval = 5 :: Float
    airplaneSpeedValue = 70 :: Float
    airplaneVerticalOffset = 6 :: Float
    airplaneHorizontalMargin = 12 :: Float
    airplaneDropsPerPass = 5 :: Int
    airplaneDropMargin = 6 :: Float
    missileReleaseOffset = 2 :: Float

    updateAirSupport :: Float -> GameState -> GameState
    updateAirSupport dt state
      | gameIsInMenu state = state { gameAirplane = Nothing, gameAirplaneCooldown = airplanePassInterval }
      | otherwise = spawnPlaneIfNeeded dt (advancePlane dt state)

    advancePlane :: Float -> GameState -> GameState
    advancePlane dt state =
      case gameAirplane state of
        Nothing -> state
        Just plane ->
          let oldX = airplaneX plane
              newX = oldX + airplaneSpeed plane * dt
              (dropsNow, remainingDrops) = collectDrops oldX newX (airplanePendingDrops plane)
              stateAfterBombs = foldl (spawnPlaneMissile plane) state dropsNow
          in if newX < airplaneEndX plane
                then stateAfterBombs { gameAirplane = Just (plane { airplaneX = newX, airplanePendingDrops = remainingDrops }) }
                else stateAfterBombs { gameAirplane = Nothing, gameAirplaneCooldown = airplanePassInterval }

    collectDrops :: Float -> Float -> [AirplaneDrop] -> ([AirplaneDrop], [AirplaneDrop])
    collectDrops oldX newX drops = (triggered, remaining)
      where
        (eligible, remaining) = span (\d -> airplaneDropX d <= newX) drops
        triggered = filter (\d -> airplaneDropX d > oldX) eligible

    spawnPlaneMissile :: AirplaneState -> GameState -> AirplaneDrop -> GameState
    spawnPlaneMissile plane state dropInfo =
      let newID = gameTotalMissileCount state
          releaseY = airplaneY plane - missileReleaseOffset
          missile = Missile
            { missileID = newID
            , missilePosition = (airplaneDropX dropInfo, releaseY)
            , missileTargetY = airplaneDropTargetY dropInfo
            , missileSpeed = missileFallSpeed
            , missileDamage = missileExplosionDamage
            , missileRadius = missileExplosionRadius
            }
      in state
          { gameMissiles = Map.insert newID missile (gameMissiles state)
          , gameTotalMissileCount = newID + 1
          }

    spawnPlaneIfNeeded :: Float -> GameState -> GameState
    spawnPlaneIfNeeded dt state
      | isJust (gameAirplane state) = state
      | otherwise =
          let newCooldown = max 0 (gameAirplaneCooldown state - dt)
              cooled = state { gameAirplaneCooldown = newCooldown }
          in if newCooldown <= 0
                then launchAirplane cooled
                else cooled

    launchAirplane :: GameState -> GameState
    launchAirplane state =
      let (stageW, stageH) = gameStageSize state
          y = stageH / 2 - airplaneVerticalOffset
          startX = -stageW / 2 - airplaneHorizontalMargin
          endX = stageW / 2 + airplaneHorizontalMargin
          passIndex = gameAirplanePassCount state + 1
          seedBase = deriveSeed (gameSeed state) passIndex
          drops = buildAirplaneDrops stageW stageH seedBase
          plane = AirplaneState
            { airplaneX = startX
            , airplaneY = y
            , airplaneSpeed = airplaneSpeedValue
            , airplaneEndX = endX
            , airplanePendingDrops = drops
            }
      in state
          { gameAirplane = Just plane
          , gameAirplaneCooldown = airplanePassInterval
          , gameAirplanePassCount = passIndex
          }

    buildAirplaneDrops :: Float -> Float -> Double -> [AirplaneDrop]
    buildAirplaneDrops stageW stageH seedBase = sortOn airplaneDropX drops
      where
        segments = fromIntegral airplaneDropsPerPass
        segmentWidth = stageW / segments
        minX = -stageW / 2 + airplaneDropMargin
        maxX = stageW / 2 - airplaneDropMargin
        minTargetY = -stageH / 2 + missileSpawnMargin
        maxTargetY = stageH / 2 - missileSpawnMargin * 2
        mkDrop idx =
          let idxf = fromIntegral idx :: Float
              idxd = fromIntegral idx :: Double
              baseCenter = -stageW / 2 + (idxf + 0.5) * segmentWidth
              jitterSeed = seedBase + idxd * 13.137
              jitter = pseudoRandomInRange jitterSeed (-segmentWidth * 0.3) (segmentWidth * 0.3)
              finalX = max minX (min maxX (baseCenter + jitter))
              targetSeed = seedBase + idxd * 29.77
              targetY = pseudoRandomInRange targetSeed minTargetY maxTargetY
          in AirplaneDrop finalX targetY
        drops = [mkDrop idx | idx <- [0 .. airplaneDropsPerPass - 1]]

    updateMissiles :: Float -> GameState -> GameState
    updateMissiles dt' state
      | Map.null (gameMissiles state) = state
      | otherwise = state
          { gameMissiles = remainingMissiles
          , gameExplosions = updatedExplosions
          , gameTotalExplosionCount = finalExplosionID
          }
      where
        (remainingMissiles, updatedExplosions, finalExplosionID) =
          Map.foldlWithKey' step (Map.empty, gameExplosions state, gameTotalExplosionCount state) (gameMissiles state)

        step (acc, explMap, nextEID) mid missile =
          let (mx, my) = missilePosition missile
              nextY = my - missileSpeed missile * dt'
          in if nextY <= missileTargetY missile
                then
                  let impactPos = (mx, missileTargetY missile)
                      newExplosion = createExplosion impactPos (missileRadius missile) (missileDamage missile) missileExplosionDuration nextEID
                  in (acc, Map.insert nextEID newExplosion explMap, nextEID + 1)
                else
                  let movedMissile = missile { missilePosition = (mx, nextY) }
                  in (Map.insert mid movedMissile acc, explMap, nextEID)

    pseudoRandomInRange :: Double -> Float -> Float -> Float
    pseudoRandomInRange seed lo hi = lo + (hi - lo) * noise seed

    noise :: Double -> Float
    noise seed = realToFrac fracPart
      where
        fracPart = frac (sin seed * 43758.5453)
        frac x = x - fromIntegral (floor x :: Int)

    defaultHazardAnimState :: HazardAnimationState
    defaultHazardAnimState = HazardAnimationState 0 0 0 False

    updateHazardAnimations :: Float -> GameState -> GameState
    updateHazardAnimations dt state = state
      { gameHazardAnimations = Map.map updateEntry pruned }
      where
        pruned = Map.filterWithKey (\hid _ -> Map.member hid (gameObstacles state)) (gameHazardAnimations state)

        updateEntry anim
          | hazardAnimPlaying anim =
              let timer' = hazardAnimTimer anim + dt
              in if timer' >= hazardAnimationFrameDuration
                    then let frame' = hazardAnimFrame anim + 1
                         in if frame' >= hazardAnimationFrameCount
                               then anim { hazardAnimFrame = 0
                                         , hazardAnimTimer = 0
                                         , hazardAnimPlaying = False
                                         , hazardAnimCooldown = hazardAnimationCooldownDuration }
                               else anim { hazardAnimFrame = frame'
                                         , hazardAnimTimer = timer' - hazardAnimationFrameDuration }
                    else anim { hazardAnimTimer = timer' }
          | hazardAnimCooldown anim > 0 =
              anim { hazardAnimCooldown = max 0 (hazardAnimCooldown anim - dt) }
          | otherwise = anim

    triggerHazardAnimation :: GameState -> ID -> GameState
    triggerHazardAnimation state hazardID =
      let anim = Map.findWithDefault defaultHazardAnimState hazardID (gameHazardAnimations state)
      in if hazardAnimPlaying anim || hazardAnimCooldown anim > 0
            then state
            else state { gameHazardAnimations = Map.insert hazardID (anim { hazardAnimPlaying = True
                                                                          , hazardAnimFrame = 0
                                                                          , hazardAnimTimer = 0 })
                                                (gameHazardAnimations state) }

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
