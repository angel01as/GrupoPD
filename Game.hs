-- Módulo que implementa el bucle principal del juego.

module Game (playGame) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Geometry

class WindowSizeState a where -- Cualquier tipo a que quiera comportarse como un estado con tamaño de ventana debe implementar estas funciones
  windowSize    :: a -> (Int, Int) -- Devuelve el tamaño de la ventana (ancho, alto)
  setWindowSize :: a -> (Int, Int) -> a -- Crea una copia del estado con el nuevo tamaño de ventana

-- TODO: Implementar en GameState (que hay que mover aquí)

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