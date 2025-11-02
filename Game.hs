-- Módulo que implementa el bucle principal del juego.
module Game (playGame) where

import Graphics.Gloss hiding (Vector, Point)
import Graphics.Gloss.Interface.Pure.Game hiding (Vector, Point)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Geometry
import Robot (Robot(..), Turret(..), MovementAction(..), MemoryValue(..), isRobotAlive, createBasicRobot, updateRobotVelocity)
import qualified Robot as R
import Entities (Projectile(..), GameEntity(..), ID, Explosion(..), updateExplosion, isExplosionActive, isExplosionDamaging, createExplosion, Obstacle(..), ObstacleType(..), ObstacleShape(..))
import qualified Entities as E
import qualified AI
import GameState
import Rendering (drawGame)
import RandomUtils (generatePositionFromSeed)
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

    newRobots = Map.fromList
      [ (rid, createBasicRobot (generatePositionFromSeed bounds seedBase rid) behavior rid)
      | (rid, behavior) <- robotInfos
      ]

    -- Obstáculos deterministas básicos al reset (10 unidades)
    newObstacles = Map.fromList [ (obstacleID o, o) | o <- generateRandomObstacles stageSize' (realToFrac seedBase) ]

playGame :: GameState -> IO ()
playGame initialState = play window backgroundColor fps initialState drawGame (handleEvents [tryHandleResizing, tryHandleMouse, tryHandleKeys]) preUpdateGame
  where
    fps = 60
    backgroundColor = white
    window = InWindow "Juego de tanques" (gameWindowSize initialState) (100, 100)

    preUpdateGame :: Float -> GameState -> GameState
    preUpdateGame dt preRealTimeGS
      | gameIsInMenu gs = gs -- La pantalla de menú se maneja solo por eventos (click)
      | Set.member (Char 'r') keys = regenerateRobotsWithRandomPositions gs initialState
      | Set.member (SpecialKey KeySpace) keys && gamePaused gs = updateGame dt (preUpdatedState { gamePaused = not (gamePaused gs), gameKeysPressed = Set.delete (SpecialKey KeySpace) keys })
      | Set.member (SpecialKey KeySpace) keys && not (gamePaused gs) = preUpdatedState { gamePaused = not (gamePaused gs), gameKeysPressed = Set.delete (SpecialKey KeySpace) keys }
      | gamePaused gs = preUpdatedState
      | otherwise = updateGame dt preUpdatedState
      where
        gs = preRealTimeGS { gameSeed = gameSeed preRealTimeGS + realToFrac dt * 12347 }
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

-- Generación determinista de obstáculos
generateRandomObstacles :: Size -> Float -> [Obstacle]
generateRandomObstacles (w,h) seed = take 4 $ map mkObs ids
  where
    bounds = (w/2, h/2)
    ids = [1001..]
    pickType rid = let p = frac (sin (seed*0.73 + fromIntegral rid*12.3) * 43758.5453)
                   in if p < 0.05 then Solid else if p < 0.20 then Hazard else if p < 0.40 then Bomb else Special
    frac x = x - fromIntegral (floor x :: Int)
    mkObs rid =
      let pos = generatePositionFromSeed bounds (realToFrac seed) rid
          t = pickType rid
          (shape, sz, col) = case t of
            Solid  -> (E.Square, (8,8), greyN 0.6)
            Hazard -> (E.Circle, (6,6), makeColor 1 0 0 0.8)
            Bomb   -> (E.Square, (4,4), makeColor 1 1 0 0.8)
            Special-> (E.Polygon (regularPolygonVerts 8 3.0), (6,6), makeColor 0.2 0.6 1.0 0.5)
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
        minX = -w/2; maxX = w/2; minY = -h/2; maxY = h/2
        cx = max minX (min maxX x)
        cy = max minY (min maxY y)
        t = (cx - x, cy - y)
        moved = if t == (0,0)
                  then e
                  else setVertices (setPosition (setVelocity e (0,0)) (cx,cy)) (translateVertices (vertices e) t)
        finalE = moved

    withinBounds :: Size -> (Float,Float) -> Bool
    withinBounds (w,h) (x,y) = x > -w/2 && x < w/2 && y > -h/2 && y < h/2

    movedProjectiles = fmap (\p -> updatePosition p deltaTime) (gameProjectiles oldState)
    keptProjectiles = Map.filter (\p -> withinBounds stageSize' (position p)) movedProjectiles

    phisicsState = oldState
      { gameProjectiles = keptProjectiles
      , gameRobots      = fmap applyPhysics (gameRobots oldState)
      , gameExplosions  = updatedExplosions
      , gameObstacles   = baseObstacles
      , gameTime        = newTime
      , gameFrame       = newFrame
      , gameCollisionCooldown = newCollisionCooldown
      }

    -- Manejo inteligente del borde del mapa (gira 90° una vez y, si persiste 0.2s, giro aleatorio 60°-120° y empuje hacia dentro)
    handleMapEdge :: Scalar -> GameState -> Robot -> Robot
    handleMapEdge dt s r =
      let (w,h) = gameStageSize s
          margin = 3 :: Float
          minX = -w/2 + margin; maxX =  w/2 - margin
          minY = -h/2 + margin; maxY =  h/2 - margin
          near = any (\(vx,vy) -> vx <= minX || vx >= maxX || vy <= minY || vy >= maxY) (vertices r)
          m = robotMemory r
          edgeCooldown = case Map.lookup "edgeCooldown" m of { Just (ScalarValue v) -> v; _ -> 0 }
          stuckTimer   = case Map.lookup "edgeStuckTimer" m of { Just (ScalarValue v) -> v; _ -> 0 }
          dec x = max 0 (x - dt)
          m' = Map.insert "edgeCooldown" (ScalarValue (dec edgeCooldown)) $ Map.insert "edgeStuckTimer" (ScalarValue (if near then stuckTimer + dt else 0)) m
      in if not near
           then r { robotMemory = Map.insert "edgeStuckTimer" (ScalarValue 0) (Map.insert "edgeCooldown" (ScalarValue (dec edgeCooldown)) m) }
           else
             let base = if edgeCooldown <= 0
                          then let rotated = updateRobotVelocity r (R.Rotate (pi/2))
                                   backed  = updateRobotVelocity rotated (R.MoveBackward 1.2)
                               in backed { robotMemory = Map.insert "edgeCooldown" (ScalarValue 0.2) m' }
                          else r { robotMemory = m' }
             in if stuckTimer + dt >= 0.2
                  then
                    let turn = AI.safeRandomTurn s r (60,120)
                        turned = updateRobotVelocity base (R.Rotate turn)
                        -- Empuje ligero hacia adentro: corrige posición si excede límites
                        (x0,y0) = position turned
                        px = (if x0 < minX then (minX - x0 + 0.3) else if x0 > maxX then (maxX - x0 - 0.3) else 0)
                        py = (if y0 < minY then (minY - y0 + 0.3) else if y0 > maxY then (maxY - y0 - 0.3) else 0)
                        push = (px, py)
                        newPos = add2D (position turned) push
                        newVerts = translateVertices (vertices turned) push
                    in setVertices (setPosition (turned { robotMemory = Map.insert "edgeStuckTimer" (ScalarValue 0) (robotMemory turned) }) newPos) newVerts
                  else base

    edgeAdjustedRobots = fmap (handleMapEdge deltaTime phisicsState) (gameRobots phisicsState)
    edgeState = phisicsState { gameRobots = edgeAdjustedRobots }

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
    maxRadius = 5 :: Float
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
                newExpl = createExplosion (position p) maxRadius explDamage maxTime newID

    -- Proyectil–Robot
    applyRobotProjectileCollisions :: [RobotProjectileCollisionEvent] -> GameState -> GameState
    applyRobotProjectileCollisions [] gs = gs
    applyRobotProjectileCollisions ((p, r):colls) gs = applyRobotProjectileCollisions colls updatedGS
      where
        updatedGS = if projectileOwnerID p == robotID r
          then gs
          else gs { gameRobots = updatedRobots
                  , gameProjectiles = Map.delete (projectileID p) (gameProjectiles gs)
                  , gameExplosions = Map.insert newID newExpl (gameExplosions gs)
                  , gameTotalExplosionCount = totalExplosionCount + 1
                  }
        totalExplosionCount = gameTotalExplosionCount gs
        robotsMap = gameRobots gs
        updatedRobots
          | isRobotAlive updatedR = Map.insert (robotID r) updatedR robotsMap
          | otherwise             = Map.delete (robotID r) robotsMap
          where
            updatedR = r { robotEnergy = robotEnergy r - projectileDamage p }
        newID = totalExplosionCount
        newExpl = createExplosion (position p) maxRadius explDamage maxTime newID

    -- Robot–Robot (solo daño cruzado sencillo)
    applyRobotRobotCollisions :: [RobotRobotCollisionEvent] -> GameState -> GameState
    applyRobotRobotCollisions [] gs = gs
    applyRobotRobotCollisions ((r1, r2):colls) gs = applyRobotRobotCollisions colls updatedGS
      where
        robotRobotDamage = 5 :: Float
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

    -- Obstáculos
    handleObstacleEffects :: GameState -> GameState
    handleObstacleEffects gs = detonateStep . hazardStep . solidStep $ gs
      where
        hazardDps = 10 :: Float

        -- Robot sólido: detener, retroceder fuerte y girar 90° (sin empujar/atravesar)
        solidStep :: GameState -> GameState
        solidStep s = foldl applySolid s robotObstacleCollisions
          where
            applySolid acc (r, o, _push)
              | obstacleType o /= Solid = acc
              | otherwise =
                  let stopped = setVelocity r (0,0)
                      backed  = updateRobotVelocity stopped (R.MoveBackward 1.5)
                      rotated = updateRobotVelocity backed (R.Rotate (pi/2))
                  in acc { gameRobots = Map.insert (robotID r) rotated (gameRobots acc) }

        hazardStep :: GameState -> GameState
        hazardStep s = foldl applyHazard s robotObstacleCollisions
          where
            applyHazard acc (r, o, _)
              | obstacleType o /= Hazard = acc
              | otherwise = acc { gameRobots = Map.adjust (\_ -> r { robotEnergy = robotEnergy r - hazardDps * deltaTime }) (robotID r) (gameRobots acc) }

        detonateStep :: GameState -> GameState
        detonateStep s0 = s3
          where
            s1 = foldl activate s0 robotObstacleCollisions
            activate acc (r, o, _)
              | obstacleType o /= Bomb = acc
              | otherwise = acc { gameObstacles = Map.adjust (\ob -> ob { obstacleTimer = Just (case obstacleTimer ob of Nothing -> 3.0; Just t -> t) }) (obstacleID o) (gameObstacles acc) }

            (s2, toExplode) = Map.foldlWithKey advance (s1, []) (gameObstacles s1)
            advance (acc, ex) oid ob = case obstacleTimer ob of
              Just t | t - deltaTime <= 0 -> (acc { gameObstacles = Map.delete oid (gameObstacles acc) }, (oid, ob):ex)
                     | otherwise          -> (acc { gameObstacles = Map.insert oid (ob { obstacleTimer = Just (t - deltaTime) }) (gameObstacles acc) }, ex)
              _ -> (acc, ex)

            s3 = foldl mkExplosion s2 toExplode
            mkExplosion acc (_, ob) =
              let totalE = gameTotalExplosionCount acc
                  eid = totalE
                  expl = createExplosion (obstaclePosition ob) 10 40 0.8 eid
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
