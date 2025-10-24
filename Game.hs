-- Módulo que implementa el bucle principal del juego.

module Game (playGame) where

import Graphics.Gloss hiding (Vector, Point)
import Graphics.Gloss.Interface.Pure.Game hiding (Vector, Point)
import Graphics.Gloss.Juicy

import Geometry
import Robot (Robot(..), Turret(..), isRobotAlive, createBasicRobot)
import Entities (Projectile(..), GameEntity(..), ID, Explosion(..), updateExplosion, isExplosionActive, isExplosionDamaging, createExplosion)
import qualified AI
import GameState
import RandomUtils (generatePositionFromSeed)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Map as Map
import qualified Data.Set as Set
import WindowSizeState
import UIButton

import Rendering

import Collisions(checkCollision, checkCollisions, RobotProjectileCollisionEvent, RobotRobotCollisionEvent)

instance WindowSizeState GameState where
  windowSize = gameWindowSize
  setWindowSize s sz = s { gameWindowSize = sz }

class KeysPressedState a where -- Cualquier tipo a que quiera comportarse como un estado con teclas presionadas debe implementar estas funciones
  keysPressed :: a -> Set.Set Key
  addKey      :: a -> Key -> a
  deleteKey   :: a -> Key -> a

instance KeysPressedState GameState where
  keysPressed   = gameKeysPressed -- Devuelve el conjunto de teclas presionadas del estado
  addKey s k    = s { gameKeysPressed = Set.insert k (gameKeysPressed s) } -- crea una copia del estado con la tecla k añadida al conjunto
  deleteKey s k = s { gameKeysPressed = Set.delete k (gameKeysPressed s) } -- crea una copia del estado con la tecla k eliminada del conjunto

instance MouseButtonState GameState where
  buttons = gameButtons

-- Controlador de eventos con Maybe
type MaybeEventHandler a = Event -> a -> Maybe a -- Recibe un evento y un estado a y devuelve Maybe a (Nothing si no maneja el evento, Just nuevoEstado si lo maneja)

handleEvents :: [MaybeEventHandler gameState] -> Event -> gameState -> gameState -- Recibe una lista de manejadores de eventos, un evento y un estado, y devuelve el nuevo estado
handleEvents handlers event gs = fromMaybe gs result -- fromMaybe: si result es Nothing devuelve gs, si es Just x devuelve x
  where
    validHandlings = [ hd event gs | hd <- handlers, not (isNothing (hd event gs)) ] -- Filtra solo los manejadores que devuelven Just (no Nothing)
    result         = if null validHandlings then Nothing else head validHandlings -- head: toma el primer elemento de la lista (el primer manejador válido)

tryHandleResizing :: (WindowSizeState wss) => Event -> wss -> Maybe wss -- Manejador de eventos para redimensionar ventana
tryHandleResizing event state =
  case event of
    EventResize size -> Just $ setWindowSize state size
    _                -> Nothing

tryHandleKeys :: (KeysPressedState kps) => Event -> kps -> Maybe kps -- Manejador de eventos para teclas
tryHandleKeys event state = -- Si el evento es una tecla presionada o liberada, actualiza el estado
  case event of
    EventKey (MouseButton btn) _ _ _ -> Nothing -- No controlamos el ratón aquí.
    EventKey k Down _ _ -> Just $ addKey state k -- Si la tecla se presiona, la añade al conjunto
    EventKey k Up   _ _ -> Just $ deleteKey state k
    _                   -> Nothing

tryHandleMouse :: (MouseButtonState mbs) => Event -> mbs -> Maybe mbs
tryHandleMouse event state = 
  case event of
    EventKey (MouseButton LeftButton) Up _ (x, y) -> Just $ handleLeftClick state (x, y) -- (x,y) es la posición del click.
    _ -> Nothing

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
  initialState { 
    gameRobots = newRobots,
    gameSimulationSpeed = gameSimulationSpeed currentState,
    gameDebugInfo = gameDebugInfo currentState,
    gameWindowSize = gameWindowSize currentState,
    gameSeed = gameSeed currentState
  }
  where
    -- Usar el tiempo actual del juego como semilla para generar nuevas posiciones
    -- Por ejemplo, si han pasado 45.7 segundos, seedBase = 45.7
    -- Esto garantiza que cada reset tenga posiciones diferentes
    seedBase = gameSeed currentState
    
    stageSize = gameStageSize initialState  -- Tamaño del escenario (ej: 100x70)
    
    -- Calcular los límites (bounds) del área jugable
    -- bounds = (mitad del ancho, mitad del alto) → si stageSize = (100, 70), entonces bounds = (50, 35)
    bounds = (fst stageSize / 2, snd stageSize / 2)
    
    -- Obtener los IDs y comportamientos de los robots originales
    originalRobots = Map.elems (gameRobots initialState)
    robotInfos = [(robotID r, robotBehavior r) | r <- originalRobots]
    
    -- Generar nuevos robots con posiciones aleatorias pero manteniendo sus IDs y comportamientos
    -- Para cada robot, generamos una nueva posición usando bounds, seedBase y su ID
    newRobots = Map.fromList [
        (rid, createBasicRobot (generatePositionFromSeed bounds seedBase rid) behavior rid)
        | (rid, behavior) <- robotInfos
      ]

playGame :: GameState -> IO()
playGame initialState = play window backgroundColor fps initialState drawGame (handleEvents [tryHandleResizing, tryHandleKeys, tryHandleMouse]) preUpdateGame
    where
        fps = 60
        backgroundColor = white
        window = InWindow "Juego de tanques " (gameWindowSize initialState) (100, 100)
        preUpdateGame :: Float -> GameState -> GameState
        preUpdateGame dt preRealTimeGS
          | gameIsInMenu gs = gameInMenu
          | Set.member (Char 'r') keys = regenerateRobotsWithRandomPositions gs initialState -- Reiniciamos el juego con posiciones aleatorias al pulsar 'r'. Usa gameTime del estado actual como semilla.
          | Set.member (SpecialKey KeySpace) keys && gamePaused gs = updateGame dt (preUpdatedState { gamePaused = (not . gamePaused) gs, gameKeysPressed = Set.delete (SpecialKey KeySpace) keys }) -- Cambiamos el modo pausa y limpiamos la tecla.
          | Set.member (SpecialKey KeySpace) keys && not (gamePaused gs) = preUpdatedState { gamePaused = (not . gamePaused) gs, gameKeysPressed = Set.delete (SpecialKey KeySpace) keys } -- Cambiamos el modo pausa y limpiamos la tecla.
          | gamePaused gs = preUpdatedState -- El juego está en pausa.
          | otherwise = updateGame dt preUpdatedState
          where
            gs = preRealTimeGS { gameSeed = gameSeed preRealTimeGS + realToFrac dt * 12347 } -- Siempre actualizamos la semilla.
            keys = gameKeysPressed gs
            preUpdatedState
              | Set.member (Char 'd') keys = gs { gameDebugInfo = not (gameDebugInfo gs), gameKeysPressed = Set.delete (Char 'd') keys } -- Cambiamos el modo debug y limpiamos la tecla.
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
            
            gameInMenu
              | Set.member (SpecialKey KeySpace) keys || Set.member (SpecialKey KeyEnter) keys = gs { gameIsInMenu = False, gameKeysPressed = Set.delete (SpecialKey KeySpace) keys }
              | otherwise = gs

-- === ACTUALIZACIÓN ===
updateGame :: Float -> GameState -> GameState
updateGame trueDeltaTime oldState = finalState { gameTime = gameTime oldState + deltaTime, gameFrame = gameFrame oldState + 1 }
  where
    deltaTime = trueDeltaTime * gameSimulationSpeed oldState
    -- Ejecutar física
    -- Comprobar las posibles colisiones (entre proyectiles, tanques, etc.).
    -- Llamar a las funciones de lógica de cada bot para determinar sus acciones.
    -- Ejecutar las actualizaciones correspondientes en los estados del juego basándose en las acciones de los bots.

    -- Primero ejecutamos las físicas con los datos antiguos.
    phisicsState = oldState
        {
            gameRobots = fmap (applyPhisics True) (gameRobots oldState),
            gameProjectiles = Map.filter (\p -> isInBounds (position p) (gameStageSize oldState)) $ fmap (applyPhisics False) (gameProjectiles oldState),
            gameExplosions = Map.filter isExplosionActive $ fmap ((flip updateExplosion) deltaTime) (gameExplosions oldState)
        }
        where
        -- Aplica la física sobre la entidad, teniendo en cuenta el deltaTime
        applyPhisics :: GameEntity a => Bool -> a -> a
        applyPhisics correctToBounds entity = finalUpdatedEntity
            where
            candidateEntity = updatePosition entity deltaTime
            finalUpdatedEntity
                | not correctToBounds || all ((flip isInBounds) (gameStageSize oldState)) (vertices candidateEntity) = candidateEntity
                | otherwise = correctedEntity
            -- Si no todos los vértices están dentro del escenario hay que corregirlo.
            correctedEntity = 
                foldr
                (\(correctCondition, correctionFunction) ent -> if correctCondition then correctionFunction ent else ent) 
                candidateEntity
                [(left, correctedLeft), (right, correctedRight), (top, correctedTop), (bottom, correctedBottom)]
                where
                    -- Calculamos el mínimo y máximo de las coordenadas de los vértices.
                    componentX = map fst (vertices candidateEntity)
                    componentY = map snd (vertices candidateEntity)
                    (minX, maxX) = (minimum componentX, maximum componentX)
                    (minY, maxY) = (minimum componentY, maximum componentY)

                    (width, height) = gameStageSize oldState
                    left = minX < -width / 2
                    right = maxX > width / 2
                    top = maxY > height / 2
                    bottom = minY < -height / 2
                    -- Velocidad anterior. Ponemos a 0 la componente que ha provocado la salida.
                    (vx, vy) = velocity candidateEntity
                    
                    correctedLeft :: GameEntity b => b -> b
                    correctedLeft ent = correctedGeneral (-width / 2 - minX, 0) (setVelocity ent (0, 0))
                    correctedRight :: GameEntity b => b -> b
                    correctedRight ent = correctedGeneral (width / 2 - maxX, 0) (setVelocity ent (0, 0))
                    correctedTop :: GameEntity b => b -> b
                    correctedTop ent = correctedGeneral (0, height / 2 - maxY) (setVelocity ent (0, 0))
                    correctedBottom :: GameEntity b => b -> b
                    correctedBottom ent = correctedGeneral (0, -height / 2 - minY) (setVelocity ent (0, 0))
                    
                    -- Traslada el objeto según la corrección.
                    correctedGeneral :: GameEntity b => Vector -> b -> b
                    correctedGeneral translation ent = setVertices (setPosition ent newPos) translatedVerts
                        where 
                        newPos = add2D (position ent) translation
                        translatedVerts = translateVertices (vertices ent) translation
    -- FIN DE APLICACIÓN DE FÍSICAS
    
    -- COLISIONES
    -- Se recuerda que los tipos son (Projectile, Robot) y (Robot, Robot)
    (robotProjectileCollisions, robotRobotCollisions) = checkCollisions (Map.elems $ gameRobots phisicsState) (Map.elems $ gameProjectiles phisicsState)
    
    -- Parámetros para nueva explosiones
    maxTime = 1
    maxRadius = 5
    explDamage = 30

    -- Gestiona daño por explosión (solo las antiguas)
    applyExplosions :: [Explosion] -> GameState -> GameState
    applyExplosions [] gs = gs
    applyExplosions (explosion:xs) gs = applyExplosions xs (applyExplosion gs)
        where
            applyExplosion :: GameState -> GameState
            applyExplosion state = 
                applyExplosionToProjectiles (Map.elems $  gameProjectiles state) $ 
                applyExplosionToRobots (Map.elems $ gameRobots state) state
                where
                    -- Comprueba si la explosión alcanza a la entidad.
                    checkExplosionGameEntity :: GameEntity a => a -> Bool
                    checkExplosionGameEntity = (checkCollision (explosionVertices explosion)).vertices
                    
                    applyExplosionToRobots :: [Robot] -> GameState -> GameState
                    applyExplosionToRobots [] gs' = gs'
                    applyExplosionToRobots (r:rs) gs'
                        | (not $ checkExplosionGameEntity r) || (not $ isExplosionDamaging explosion) = applyExplosionToRobots rs gs' -- Importa el orden de definición.
                        | isRobotAlive updatedRobot = applyExplosionToRobots rs (gs' { gameRobots = Map.insert (robotID updatedRobot) updatedRobot (gameRobots gs') })
                        | otherwise = applyExplosionToRobots rs (gs' { gameRobots = Map.delete (robotID updatedRobot) (gameRobots gs')})
                        where updatedRobot = r { robotEnergy = (robotEnergy r - deltaTime * explosionDamage explosion) }
                    
                    applyExplosionToProjectiles :: [Projectile] -> GameState -> GameState
                    applyExplosionToProjectiles [] gs' = gs'
                    applyExplosionToProjectiles (p:ps) gs'
                        | checkExplosionGameEntity p && isExplosionDamaging explosion = applyExplosionToProjectiles ps updatedGS
                        | otherwise = applyExplosionToProjectiles ps gs'
                        where
                            totalExplosionCount = gameTotalExplosionCount gs'
                            -- Si colisiona con un proyectil, se destruye y se genera una nueva explosión.
                            updatedGS = gs' { 
                                                gameProjectiles = Map.delete (projectileID p) (gameProjectiles gs'),
                                                gameExplosions = Map.insert newID newExpl (gameExplosions gs'),
                                                gameTotalExplosionCount = totalExplosionCount + 1
                                            }
                            newID = totalExplosionCount
                            newExpl = createExplosion (position p) maxRadius explDamage maxTime newID

    -- Gestiona colisiones entre Proyectil y Robot, generando una nueva explosión.
    applyRobotProjectileCollisions :: [RobotProjectileCollisionEvent] -> GameState -> GameState
    applyRobotProjectileCollisions [] gs = gs
    applyRobotProjectileCollisions ((p, r):colls) gs = applyRobotProjectileCollisions colls updatedGS
        where
            -- Si el Robot no es el dueño del proyectil:
            -- Dañamos al Robot, destruimos el proyectil y generamos una explosión.
            updatedGS = if projectileOwnerID p == robotID r
                then gs
                else gs { 
                            gameRobots = updatedRobots,
                            gameProjectiles = Map.delete (projectileID p) (gameProjectiles gs),
                            gameExplosions = Map.insert newID newExpl (gameExplosions gs),
                            gameTotalExplosionCount = totalExplosionCount + 1
                        }
                where
                    totalExplosionCount = gameTotalExplosionCount gs
                    robots = gameRobots gs
                    updatedRobots
                        | isRobotAlive updatedR = Map.insert (robotID r) updatedR robots
                        | otherwise = Map.delete (robotID r) robots
                        where
                            updatedR = r { robotEnergy = (robotEnergy r - projectileDamage p) }
                    newID = totalExplosionCount
                    newExpl = createExplosion (position p) maxRadius explDamage maxTime newID

    -- Gestiona colisiones entre Robot y Robot, dañando a ambos por igual.
    applyRobotRobotCollisions :: [RobotRobotCollisionEvent] -> GameState -> GameState
    applyRobotRobotCollisions [] gs = gs
    applyRobotRobotCollisions ((r1, r2):colls) gs = applyRobotRobotCollisions colls updatedGS
        where
            -- Parámetro de daño, se puede cambiar para que, por ejemplo, considere la velocidad de choque.
            robotRobotDamage = 5
            updatedGS = gs { gameRobots = adjustRobot r1 $ adjustRobot r2 (gameRobots gs)}
                where
                    -- Quita vida al Robot lo actualiza/elimina según proceda.
                    adjustRobot :: Robot -> Map.Map ID Robot -> Map.Map ID Robot
                    adjustRobot r robots
                        | isRobotAlive updatedR = Map.insert (robotID r) updatedR robots
                        | otherwise = Map.delete (robotID r) robots
                        where
                            -- Se puede mejorar, por ejemplo, reduciendo la velocidad.
                            updatedR = r { robotEnergy = (robotEnergy r - robotRobotDamage * deltaTime) } 
    -- Aplicamos todas las colisiones y explosiones
    collisionState = 
        applyRobotRobotCollisions robotRobotCollisions $
        applyRobotProjectileCollisions robotProjectileCollisions $
        applyExplosions (Map.elems $ gameExplosions phisicsState) phisicsState
    -- FIN DE COLISIONES

    -- IA --

    -- Actualizamos cada robot con su IA y recogemos nuevos proyectiles
    aiResults = fmap (\r -> AI.updateRobotAI r collisionState deltaTime) (gameRobots collisionState)
    updatedRobots = fmap AI.updatedRobot aiResults
    spawnedProjectiles = concatMap AI.newProjectiles aiResults
    
    insertSpawnedProjectiles :: ID -> Map.Map ID Projectile -> [Projectile] -> Map.Map ID Projectile
    insertSpawnedProjectiles nextID m [] = m
    insertSpawnedProjectiles nextID m (p:ps) = insertSpawnedProjectiles (nextID + 1) newM ps
        where
            newM = Map.insert nextID (p { projectileID = nextID }) m
    -- Metemos cada proyectil con ID = Total histórico de proyectiles. 
    totalProjectileCount = gameTotalProjectileCount collisionState
    allProjectiles = insertSpawnedProjectiles totalProjectileCount (gameProjectiles collisionState) spawnedProjectiles
    -- FIN DE IA --

    finalState = collisionState 
      { 
        gameRobots = updatedRobots, gameProjectiles = allProjectiles, 
        gameTotalProjectileCount = totalProjectileCount + length spawnedProjectiles
      }
