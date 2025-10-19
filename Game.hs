-- Módulo que implementa el bucle principal del juego.

module Game (playGame, loadBackgroundImage) where

import Graphics.Gloss hiding (Vector, Point)
import Graphics.Gloss.Interface.Pure.Game hiding (Vector, Point)
import Graphics.Gloss.Juicy

import Geometry
import Robot (Robot(..), Turret(..), isRobotAlive)
import Entities (Projectile(..), GameEntity(..), ID, Explosion(..), updateExplosion, isExplosionActive, isExplosionDamaging, createExplosion)
import qualified AI
import GameState
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Collisions(checkCollision, checkCollisions, RobotProjectileCollisionEvent, RobotRobotCollisionEvent)

class WindowSizeState a where -- Cualquier tipo a que quiera comportarse como un estado con tamaño de ventana debe implementar estas funciones
  windowSize    :: a -> (Int, Int) -- Devuelve el tamaño de la ventana (ancho, alto)
  setWindowSize :: a -> (Int, Int) -> a -- Crea una copia del estado con el nuevo tamaño de ventana

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
    EventKey k Down _ _ -> Just $ addKey state k -- Si la tecla se presiona, la añade al conjunto
    EventKey k Up   _ _ -> Just $ deleteKey state k
    _                   -> Nothing

-- gmap es un fmap sobre un Map que devuelve el resultado como lista.
gmap :: (Ord k) => (v -> w) -> Map.Map k v -> [w]
gmap f m = Map.elems (fmap f m)

-- gfilter es un filter sobre un Map que devuelve el resultado como lista.
gfilter :: (Ord k) => (v -> Bool) -> Map.Map k v -> [v]
gfilter f m = Map.elems (Map.filter f m)

-- Dimensiones base del juego. Usar Scalar para cáculos internos y Pixel para la pantalla., incluso si ambos son Float.

type Pixel = Float

-- Factor de escalado dinámico basado en el tamaño de ventana
baseWindowSize :: (Int, Int)
baseWindowSize = (1000, 700)

-- Calcula el factor de escalado dinámico
getMeter2PixelFactor :: GameState -> Pixel
getMeter2PixelFactor gs = 
  min (fromIntegral windowWidth / sceneWidth)
      (fromIntegral windowHeight / sceneHeight)
      where
        (windowWidth, windowHeight) = gameWindowSize gs
        (sceneWidth, sceneHeight) = gameStageSize gs

getPixel2MeterFactor :: GameState -> Scalar
getPixel2MeterFactor gs = 1 / getMeter2PixelFactor gs

pixel2Meter :: GameState -> Pixel -> Scalar
pixel2Meter gs px = px * getPixel2MeterFactor gs

meter2Pixel :: GameState -> Scalar -> Pixel
meter2Pixel gs m = m * getMeter2PixelFactor gs


playGame :: GameState -> IO()
playGame initialState = play window backgroundColor fps initialState drawGame (handleEvents [tryHandleResizing, tryHandleKeys]) preUpdateGame
    where
        fps = 60
        backgroundColor = white
        window = InWindow "Juego de tanques " (gameWindowSize initialState) (100, 100)
        preUpdateGame :: Float -> GameState -> GameState
        preUpdateGame dt gs
          | Set.member (Char 'r') (gameKeysPressed gs) = initialState { gameSimulationSpeed = gameSimulationSpeed gs, gameDebugInfo = gameDebugInfo gs, gameWindowSize = gameWindowSize gs} -- Reiniciamos el juego al pulsar 'r'. Nótese que initialState no tiene 'r' pulsado y no se volverá a añadir hasta que se suelte y vuelva pulsar la tecla.
          | Set.member (Char 'd') keys = updateGame dt (gs { gameDebugInfo = not (gameDebugInfo gs), gameKeysPressed = Set.delete (Char 'd') keys }) -- Cambiamos el modo debug y limpiamos la tecla.
          | Set.member (Char '1') keys = updateGame dt (gs { gameSimulationSpeed = 0.1 }) -- Cambiamos la velocidad de simulación.
          | Set.member (Char '2') keys = updateGame dt (gs { gameSimulationSpeed = 0.25 })
          | Set.member (Char '3') keys = updateGame dt (gs { gameSimulationSpeed = 0.5 })
          | Set.member (Char '4') keys = updateGame dt (gs { gameSimulationSpeed = 0.75 })
          | Set.member (Char '5') keys = updateGame dt (gs { gameSimulationSpeed = 1.0 })
          | Set.member (Char '6') keys = updateGame dt (gs { gameSimulationSpeed = 1.25 })
          | Set.member (Char '7') keys = updateGame dt (gs { gameSimulationSpeed = 1.5 })
          | Set.member (Char '8') keys = updateGame dt (gs { gameSimulationSpeed = 2.0 })
          | Set.member (Char '9') keys = updateGame dt (gs { gameSimulationSpeed = 2.5 })
          | Set.member (Char '0') keys = updateGame dt (gs { gameSimulationSpeed = 3.0 })
          | otherwise                  = updateGame dt gs
          where
            keys = gameKeysPressed gs

-- === CARGA DE IMÁGENES ===
-- Función para cargar imagen de fondo (retorna Nothing si falla)
loadBackgroundImage :: String -> IO (Maybe Picture)
loadBackgroundImage path = loadJuicy path

-- === DIBUJO ===
drawGame :: GameState -> Picture
drawGame gs
  | gameDebugInfo gs = Pictures [regularGameInfo, debugGameInfo]
  | otherwise = regularGameInfo
  where
    regularGameInfo = Pictures [background, robotsPic, projectilesPic, explosionsPic, ui]
    debugGameInfo = Pictures [createBorder, drawAllVertices]
    windowSize = gameWindowSize gs
    (windowWidth, windowHeight) = (fromIntegral (fst windowSize), fromIntegral (snd windowSize))
    (baseWindowWidth, baseWindowHeight) = (fromIntegral $ fst baseWindowSize, fromIntegral $ snd baseWindowSize)
    
    createBorder :: Picture
    createBorder = Color red $ rectangleWire (meter2Pixel gs sx) (meter2Pixel gs sy) where (sx, sy) = gameStageSize gs    

    -- Usar imagen de fondo si está disponible, sino usar color sólido
    background = case gameBackground gs of
      Just bgImage -> Scale (windowWidth / baseWindowWidth) (windowHeight / baseWindowHeight) bgImage
      Nothing      -> Color (makeColor 0.1 0.1 0.1 1.0) $ rectangleSolid windowWidth windowHeight
    
    robotsPic = Pictures (gmap drawRobot (gameRobots gs))
    projectilesPic = Pictures (gmap drawProjectile (gameProjectiles gs))
    explosionsPic = Pictures (gmap drawExplosion (gameExplosions gs))
    ui = drawUI gs

    -- Convierte coordenadas del mundo a píxeles
    toPx :: (Scalar, Scalar) -> (Float, Float)
    toPx (x, y) = (meter2Pixel gs x, meter2Pixel gs y)

    -- Convierte ángulos de radianes a grados para Gloss
    radToDeg :: Angle -> Float
    radToDeg = rad2deg

    -- Renderiza un robot completo (tanque + cañón + barra de salud)
    drawRobot :: Robot -> Picture
    drawRobot r = Pictures [tank, turret, healthBar]
      where
        (x, y) = position r
        (sx, sy) = size r
        robotOrientation = -radToDeg (orientation r) -- Gloss usa rotación en sentido horario expresada en grados y nosotros rotación antihoraria expresada en radianes.
        turretAngle = -radToDeg (turretOrientation (robotTurret r))
        
        -- Tanque (cuerpo del robot)
        tank = Color (if isRobotAlive r then (makeColor 0.2 0.4 0.2 1.0) else (makeColor 0.5 0.5 0.5 1.0)) $
               Translate (meter2Pixel gs x) (meter2Pixel gs y) $
               Rotate robotOrientation $
               rectangleSolid (meter2Pixel gs sx) (meter2Pixel gs sy)
        
        -- Cañón (torreta)
        turretLength = meter2Pixel gs sx * 1.5  -- Cañón más largo
        turretWidth = meter2Pixel gs sy * 0.3   -- Ancho más grueso
        turret = Color (makeColor 0.1 0.2 0.1 1.0) $
                 Translate (meter2Pixel gs x) (meter2Pixel gs y) $
                 Rotate turretAngle $
                 Translate (turretLength/4) 0 $
                 rectangleSolid (turretLength/2) (turretWidth/2)
        
        -- Barra de salud
        healthBar = drawHealthBar r

    -- Renderiza la barra de salud de un robot
    drawHealthBar :: Robot -> Picture
    drawHealthBar r = Pictures [background, health]
      where
        (x, y) = position r
        (sx, sy) = size r
        
        -- Posición de la barra (arriba del robot)
        barY = meter2Pixel gs y + meter2Pixel gs sy/2 + 15
        barWidth = meter2Pixel gs sx * 1.2
        barHeight = 6
        
        -- Porcentaje de salud
        healthPercent = robotEnergy r / robotMaxEnergy r
        
        -- Fondo de la barra (rojo)
        background = Color red $
                     Translate (meter2Pixel gs x) barY $
                     rectangleSolid (barWidth/2) (barHeight/2)
        
        -- Barra de salud (verde)
        health = Color green $
                 Translate (meter2Pixel gs x - barWidth/4 + (barWidth/2 * healthPercent)/2) barY $
                 rectangleSolid (barWidth/2 * healthPercent) (barHeight/2)

    -- Renderiza un proyectil (círculo)
    drawProjectile :: Projectile -> Picture
    drawProjectile p = Color orange $ 
      uncurry Translate (toPx (position p)) $ -- uncurry convierte tupla (x,y) en argumentos separados para Translate
      circleSolid (meter2Pixel gs 0.5) -- Radio de 0.5 metros

    drawExplosion :: Explosion -> Picture
    drawExplosion e = Color (withAlpha 0.7 red) $ 
        uncurry Translate (toPx (explosionPosition e)) $ 
        circleSolid (meter2Pixel gs (explosionRadius e))

    -- Renderiza la interfaz de usuario
    drawUI :: GameState -> Picture
    drawUI gs = Pictures [timeDisplay, robotCount, hotkeys]
      where

        -- Posiciones responsive basadas en el tamaño de ventana
        leftMargin = -windowWidth/2 + 20
        topMargin = windowHeight/2 - 20
        
        -- Escala responsive basada en el tamaño de ventana
        textScale = min (windowWidth / baseWindowWidth) (windowHeight / baseWindowHeight) * 0.2 -- min: toma el menor de los dos valores para mantener proporciones
        
        timeDisplay = Color white $ Translate leftMargin (topMargin - 30) $ Scale textScale textScale $ 
                      Text ("Tiempo: " ++ show (round (gameTime gs) :: Int) ++ "s")
        
        robotCount = Color white $ Translate leftMargin (topMargin - 60) $ Scale textScale textScale $ 
                     Text ("Robots vivos: " ++ show (length (gameRobots gs)))
        hotkeys = Color white $ Translate leftMargin (topMargin - 90) $ Scale textScale textScale $
                      Text "Reset: R | Debug: D | Velocidad: 1..0"
    
    drawAllVertices :: Picture
    drawAllVertices = Pictures vertsDrawings
      where
        allVertices = concatMap vertices (gameRobots gs) ++ concatMap vertices (gameProjectiles gs)
        vertsDrawings = drawVertices allVertices
          where
            drawVertices :: [Point] -> [Picture]
            drawVertices [] = []
            drawVertices (v:vs) = vertDraw:drawVertices vs
              where vertDraw = Color red $ uncurry Translate (toPx v) $ circleSolid (meter2Pixel gs 0.5)

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
                    correctedLeft ent = correctedGeneral (-width / 2 - minX, 0) (setVelocity ent (0, vy))
                    correctedRight :: GameEntity b => b -> b
                    correctedRight ent = correctedGeneral (width / 2 - maxX, 0) (setVelocity ent (0, vy))
                    correctedTop :: GameEntity b => b -> b
                    correctedTop ent = correctedGeneral (0, height / 2 - maxY) (setVelocity ent (vx, 0))
                    correctedBottom :: GameEntity b => b -> b
                    correctedBottom ent = correctedGeneral (0, -height / 2 - minY) (setVelocity ent (vx, 0))
                    
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
