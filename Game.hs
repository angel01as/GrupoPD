-- Módulo que implementa el bucle principal del juego.

module Game (GameState(..), playGame, loadBackgroundImage) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy

import Geometry
import Robot (Robot(..), isRobotAlive, Turret(..))
import Entities (Projectile(..), GameEntity(..))
import qualified AI
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Map as Map

class WindowSizeState a where -- Cualquier tipo a que quiera comportarse como un estado con tamaño de ventana debe implementar estas funciones
  windowSize    :: a -> (Int, Int) -- Devuelve el tamaño de la ventana (ancho, alto)
  setWindowSize :: a -> (Int, Int) -> a -- Crea una copia del estado con el nuevo tamaño de ventana

-- Estado principal del juego (para el bucle Gloss)
data GameState = GS
  { gsWindowSize   :: (Int, Int)
  , gsRobots       :: [Robot]
  , gsProjectiles  :: [Projectile]
  , gsTime         :: Scalar
  , gsBackground   :: Maybe Picture  -- Imagen de fondo opcional
  }

instance WindowSizeState GameState where
  windowSize = gsWindowSize
  setWindowSize s sz = s { gsWindowSize = sz }

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

-- Dimensiones base del juego

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
drawGame gs = Pictures [background, robotsPic, projectilesPic, ui]
  where
    windowSize = gsWindowSize gs
    (windowWidth, windowHeight) = (fromIntegral (fst windowSize), fromIntegral (snd windowSize)) -- fst/snd: extraen ancho y alto de la tupla (Int,Int)
    
    -- Usar imagen de fondo si está disponible, sino usar color sólido
    background = case gsBackground gs of
      Just bgImage -> Scale (windowWidth / 1000) (windowHeight / 700) bgImage
      Nothing      -> Color (makeColor 0.1 0.1 0.1 1.0) $ rectangleSolid windowWidth windowHeight
    
    robotsPic = Pictures (map (drawRobot windowSize) (gsRobots gs))
    projectilesPic = Pictures (map (drawProjectile windowSize) (gsProjectiles gs))
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
        windowSize = gsWindowSize gs
        (windowWidth, windowHeight) = (fromIntegral (fst windowSize), fromIntegral (snd windowSize)) -- fst/snd: extraen ancho y alto de la tupla (Int,Int)
        
        -- Posiciones responsive basadas en el tamaño de ventana
        leftMargin = -windowWidth/2 + 20
        topMargin = windowHeight/2 - 20
        
        -- Escala responsive basada en el tamaño de ventana
        textScale = min (windowWidth/1000) (windowHeight/700) * 0.2 -- min: toma el menor de los dos valores para mantener proporciones
        
        timeDisplay = Color white $ Translate leftMargin (topMargin - 30) $ Scale textScale textScale $ 
                      Text ("Tiempo: " ++ show (round (gsTime gs) :: Int) ++ "s")
        
        robotCount = Color white $ Translate leftMargin (topMargin - 60) $ Scale textScale textScale $ 
                     Text ("Robots vivos: " ++ show (length (filter isRobotAlive (gsRobots gs))))

-- === ACTUALIZACIÓN ===
updateGame :: Float -> GameState -> GameState
updateGame dt gs =
  let delta = realToFrac dt :: Scalar
      -- Construimos el estado AI para la toma de decisiones de cada robot
      aiState :: AI.GameState
      aiState = AI.GameState { AI.gameRobots = gsRobots gs
                             , AI.gameProjectiles = gsProjectiles gs
                             , AI.gameTime = gsTime gs }

      -- Actualizamos cada robot con su IA y recogemos nuevos proyectiles
      aiResults = map (\r -> AI.updateRobotAI r aiState delta) (gsRobots gs)
      updatedRobots = map AI.updatedRobot aiResults
      spawnedProjectiles = concatMap AI.newProjectiles aiResults

      -- Avanzamos físicamente posiciones según velocidades
      movedRobots = map (\r -> updatePosition r delta) updatedRobots
      movedProjectiles = map (\p -> updatePosition p delta) (spawnedProjectiles ++ gsProjectiles gs)

  in gs { gsRobots = movedRobots
        , gsProjectiles = movedProjectiles
        , gsTime = gsTime gs + delta }