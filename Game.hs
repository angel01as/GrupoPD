-- Módulo que implementa el bucle principal del juego.

module Game (playGame, loadBackgroundImage) where

import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Interface.Pure.Game hiding (Vector)
import Graphics.Gloss.Juicy

import Geometry
import Robot (Robot(..), Turret(..), isRobotAlive)
import Entities (Projectile(..), GameEntity(..), ID)
import qualified AI
import GameState
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Map as Map

class WindowSizeState a where -- Cualquier tipo a que quiera comportarse como un estado con tamaño de ventana debe implementar estas funciones
  windowSize    :: a -> (Int, Int) -- Devuelve el tamaño de la ventana (ancho, alto)
  setWindowSize :: a -> (Int, Int) -> a -- Crea una copia del estado con el nuevo tamaño de ventana

instance WindowSizeState GameState where
  windowSize = gameWindowSize
  setWindowSize s sz = s { gameWindowSize = sz }

-- Controlador de eventos con Maybe
type MaybeEventHandler a = Event -> a -> Maybe a -- Recibe un evento y un estado a y devuelve Maybe a (Nothing si no maneja el evento, Just nuevoEstado si lo maneja)

handleEvents :: [MaybeEventHandler gameState] -> Event -> gameState -> gameState -- Recibe una lista de manejadores de eventos, un evento y un estado, y devuelve el nuevo estado
handleEvents handlers event gs = fromMaybe gs result -- Si result es Nothing devuelve el estado original gs, si es Just nuevoEstado devuelve nuevoEstado
  where
    validHandlings = [ hd event gs | hd <- handlers, not (isNothing (hd event gs)) ]  
    result         = if null validHandlings then Nothing else head validHandlings -- Intenta aplicar cada manejador de eventos al evento y al estado, y devuelve, en su caso, el resultado del primero que sea compatible

tryHandleResizing :: (WindowSizeState wss) => Event -> wss -> Maybe wss -- Manejador de eventos para redimensionar ventana
tryHandleResizing event state =
  case event of
    EventResize size -> Just $ setWindowSize state size
    _                -> Nothing

-- gmap es un fmap sobre un Map que devuelve el resultado como lista.
gmap :: (Ord k) => (v -> w) -> Map.Map k v -> [w]
gmap f m = Map.elems (fmap f m)

-- gfilter es un filter sobre un Map que devuelve el resultado como lista.
gfilter :: (Ord k) => (v -> Bool) -> Map.Map k v -> [v]
gfilter f m = Map.elems (Map.filter f m)
-- Dimensiones base del juego. Usar Scalar para cáculos internos y Pixel para la pantalla., incluso si ambos son Float.

type Pixel = Float

-- Factor de escalado dinámico basado en el tamaño de ventana
-- Escenario base: 1000x700 píxeles = 100x70 metros
baseWindowSize :: (Int, Int)
baseWindowSize = (1000, 700)

baseSceneSize :: (Scalar, Scalar) -- metros
baseSceneSize = (100, 70)

-- Calcula el factor de escalado dinámico
getMeter2PixelFactor :: (Int, Int) -> Pixel
getMeter2PixelFactor (windowWidth, windowHeight) = 
  min (fromIntegral windowWidth / fst baseSceneSize) -- fst: toma el primer elemento de la tupla (ancho)
      (fromIntegral windowHeight / snd baseSceneSize) -- snd: toma el segundo elemento de la tupla (alto)

getPixel2MeterFactor :: (Int, Int) -> Scalar
getPixel2MeterFactor windowSize = 1 / getMeter2PixelFactor windowSize

pixel2Meter :: (Int, Int) -> Pixel -> Scalar
pixel2Meter windowSize px = px * getPixel2MeterFactor windowSize

meter2Pixel :: (Int, Int) -> Scalar -> Pixel
meter2Pixel windowSize m = m * getMeter2PixelFactor windowSize


playGame :: GameState -> IO()
playGame initialState = play window backgroundColor fps initialState drawGame (handleEvents [tryHandleResizing]) updateGame
    where
        fps = 60
        backgroundColor = white
        window = InWindow "Juego de tanques " (windowSize initialState) (100, 100)

-- === CARGA DE IMÁGENES ===
-- Función para cargar imagen de fondo (retorna Nothing si falla)
loadBackgroundImage :: String -> IO (Maybe Picture)
loadBackgroundImage path = loadJuicy path

-- === DIBUJO ===
drawGame :: GameState -> Picture
drawGame gs = Pictures [background, robotsPic, projectilesPic, ui, createBorder]
  where
    windowSize = gameWindowSize gs
    (windowWidth, windowHeight) = (fromIntegral (fst windowSize), fromIntegral (snd windowSize)) -- fst/snd: extraen ancho y alto de la tupla (Int,Int)
    
    createBorder :: Picture
    createBorder = Color red $ rectangleWire (meter2Pixel windowSize sx) (meter2Pixel windowSize sy) where (sx, sy) = gameStageSize gs    

    -- Usar imagen de fondo si está disponible, sino usar color sólido
    background = case gameBackground gs of
      Just bgImage -> Scale (windowWidth / 1000) (windowHeight / 700) bgImage
      Nothing      -> Color (makeColor 0.1 0.1 0.1 1.0) $ rectangleSolid windowWidth windowHeight
    
    robotsPic = Pictures (gmap (drawRobot windowSize) (gameRobots gs))
    projectilesPic = Pictures (gmap (drawProjectile windowSize) (gameProjectiles gs))
    ui = drawUI gs

    -- Convierte coordenadas del mundo a píxeles
    toPx :: (Scalar, Scalar) -> (Float, Float)
    toPx (x, y) = (meter2Pixel windowSize x, meter2Pixel windowSize y)

    -- Convierte ángulos de radianes a grados para Gloss
    radToDeg :: Angle -> Float
    radToDeg = rad2deg

    -- Renderiza un robot completo (tanque + cañón + barra de salud)
    drawRobot :: (Int, Int) -> Robot -> Picture
    drawRobot windowSize r = Pictures [tank, turret, healthBar]
      where
        (x, y) = position r
        (sx, sy) = size r
        robotOrientation = radToDeg (orientation r)
        turretAngle = radToDeg (turretOrientation (robotTurret r))
        
        -- Tanque (cuerpo del robot)
        tank = Color (if isRobotAlive r then (makeColor 0.2 0.4 0.2 1.0) else (makeColor 0.5 0.5 0.5 1.0)) $
               Translate (meter2Pixel windowSize x) (meter2Pixel windowSize y) $
               Rotate robotOrientation $
               rectangleSolid (meter2Pixel windowSize sx) (meter2Pixel windowSize sy)
        
        -- Cañón (torreta)
        turretLength = meter2Pixel windowSize sx * 1.5  -- Cañón más largo
        turretWidth = meter2Pixel windowSize sy * 0.3   -- Ancho más grueso
        turret = Color (makeColor 0.1 0.2 0.1 1.0) $
                 Translate (meter2Pixel windowSize x) (meter2Pixel windowSize y) $
                 Rotate turretAngle $
                 Translate (turretLength/4) 0 $
                 rectangleSolid (turretLength/2) (turretWidth/2)
        
        -- Barra de salud
        healthBar = drawHealthBar windowSize r

    -- Renderiza la barra de salud de un robot
    drawHealthBar :: (Int, Int) -> Robot -> Picture
    drawHealthBar windowSize r = Pictures [background, health]
      where
        (x, y) = position r
        (sx, sy) = size r
        
        -- Posición de la barra (arriba del robot)
        barY = meter2Pixel windowSize y + meter2Pixel windowSize sy/2 + 15
        barWidth = meter2Pixel windowSize sx * 1.2
        barHeight = 6
        
        -- Porcentaje de salud
        healthPercent = robotEnergy r / robotMaxEnergy r
        
        -- Fondo de la barra (rojo)
        background = Color red $
                     Translate (meter2Pixel windowSize x) barY $
                     rectangleSolid (barWidth/2) (barHeight/2)
        
        -- Barra de salud (verde)
        health = Color green $
                 Translate (meter2Pixel windowSize x - barWidth/4 + (barWidth/2 * healthPercent)/2) barY $
                 rectangleSolid (barWidth/2 * healthPercent) (barHeight/2)

    -- Renderiza un proyectil (círculo)
    drawProjectile :: (Int, Int) -> Projectile -> Picture
    drawProjectile windowSize p = Color orange $ 
      uncurry Translate (toPx (position p)) $ -- uncurry convierte tupla (x,y) en argumentos separados para Translate
      circleSolid (meter2Pixel windowSize 0.5) -- Radio de 0.5 metros

    -- Renderiza la interfaz de usuario
    drawUI :: GameState -> Picture
    drawUI gs = Pictures [timeDisplay, robotCount]
      where
        windowSize = gameWindowSize gs
        (windowWidth, windowHeight) = (fromIntegral (fst windowSize), fromIntegral (snd windowSize)) -- fst/snd: extraen ancho y alto de la tupla (Int,Int)
        
        -- Posiciones responsive basadas en el tamaño de ventana
        leftMargin = -windowWidth/2 + 20
        topMargin = windowHeight/2 - 20
        
        -- Escala responsive basada en el tamaño de ventana
        textScale = min (windowWidth/1000) (windowHeight/700) * 0.2 -- min: toma el menor de los dos valores para mantener proporciones
        
        timeDisplay = Color white $ Translate leftMargin (topMargin - 30) $ Scale textScale textScale $ 
                      Text ("Tiempo: " ++ show (round (gameTime gs) :: Int) ++ "s")
        
        robotCount = Color white $ Translate leftMargin (topMargin - 60) $ Scale textScale textScale $ 
                     Text ("Robots vivos: " ++ show (length (gfilter isRobotAlive (gameRobots gs))))

-- === ACTUALIZACIÓN ===
updateGame :: Float -> GameState -> GameState
updateGame deltaTime oldState = finalState { gameTime = gameTime oldState + deltaTime }
  where
    -- Ejecutar física
    -- Comprobar las posibles colisiones (entre proyectiles, tanques, etc.).
    -- Llamar a las funciones de lógica de cada bot para determinar sus acciones.
    -- Ejecutar las actualizaciones correspondientes en los estados del juego basándose en las acciones de los bots.

    -- Primero ejecutamos las físicas con los datos antiguos.
    phisicsState = oldState
        {
            gameRobots = fmap applyPhisics (gameRobots oldState),
            gameProjectiles = fmap applyPhisics (gameProjectiles oldState)
        }
        -- TODO: Faltan las explosiones.
        where
        -- Aplica la física sobre la entidad, teniendo en cuenta el deltaTime
        applyPhisics :: GameEntity a => a -> a
        applyPhisics entity = finalUpdatedEntity
            where
            candidateEntity = updatePosition entity deltaTime
            finalUpdatedEntity
                | all ((flip isInBounds) (gameStageSize oldState)) (vertices candidateEntity) = candidateEntity
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
    
    -- COLISIONES (Pendiente)
    collisionState = phisicsState
    -- FIN DE COLISIONES

    -- IA --

    -- Adaptado de Ángel. Pendiente de mejora.
    -- Actualizamos cada robot con su IA y recogemos nuevos proyectiles
    aiResults = fmap (\r -> AI.updateRobotAI r collisionState deltaTime) (gameRobots collisionState)
    updatedRobots = fmap AI.updatedRobot aiResults
    spawnedProjectiles = concatMap AI.newProjectiles aiResults
    
    insertSpawnedProjectiles :: Map.Map ID Projectile -> [Projectile] -> Map.Map ID Projectile
    insertSpawnedProjectiles m [] = m
    insertSpawnedProjectiles m (x:xs) = insertSpawnedProjectiles newM xs
        where
            newID = Map.size m
            newM = Map.insert newID (x { projectileID = newID }) m
    -- Metemos cada proyectil con ID = tamaño de m 

    allProjectiles = insertSpawnedProjectiles (gameProjectiles collisionState) spawnedProjectiles
    -- FIN DE IA --

    finalState = collisionState { gameRobots = updatedRobots, gameProjectiles = allProjectiles}
