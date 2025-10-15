-- Módulo que implementa el bucle principal del juego.

module Game (GameState(..), playGame) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Geometry
import Robot (Robot(..))
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
  }

instance WindowSizeState GameState where
  windowSize = gsWindowSize
  setWindowSize s sz = s { gsWindowSize = sz }

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

-- Dimensiones base del juego

type Pixel = Float

meter2PixelFactor :: Pixel
meter2PixelFactor = 25

pixel2MeterFactor :: Scalar
pixel2MeterFactor = 1 / meter2PixelFactor

pixel2Meter :: Pixel -> Scalar
pixel2Meter px = px * pixel2MeterFactor

meter2Pixel :: Scalar -> Pixel
meter2Pixel m = m * meter2PixelFactor


playGame :: GameState -> IO()
playGame initialState = play window backgroundColor fps initialState drawGame (handleEvents [tryHandleResizing]) updateGame
    where
        fps = 60
        backgroundColor = white
        window = InWindow "Juego de tanques " (windowSize initialState) (100, 100)

-- === DIBUJO ===
drawGame :: GameState -> Picture
drawGame gs = Pictures [robotsPic, projectilesPic]
  where
    robotsPic = Pictures (map drawRobot (gsRobots gs))
    projectilesPic = Pictures (map drawProjectile (gsProjectiles gs))

    drawRobot :: Robot -> Picture
    drawRobot r =
      let (x, y) = position r
          (w, h) = size r
      in Color (makeColorI 60 130 240 255)
         $ Translate (meter2Pixel x) (meter2Pixel y)
         $ rectangleSolid (meter2Pixel w) (meter2Pixel h)

    drawProjectile :: Projectile -> Picture
    drawProjectile p = let (x, y) = position p in
      Color (makeColorI 240 80 80 255) $ Translate (meter2Pixel x) (meter2Pixel y) $ Polygon (map toPx (vertices p))

    toPx :: (Scalar, Scalar) -> (Float, Float)
    toPx (x, y) = (meter2Pixel x, meter2Pixel y)

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