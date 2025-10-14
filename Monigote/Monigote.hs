module Monigote where

import Graphics.Gloss -- dibuja (Picture, Translate, Rotate, Color, etc
import Graphics.Gloss.Interface.Pure.Game -- para el juego
import qualified Data.Set as Sets hiding (Set (show)) -- para el set de teclas presionadas
import Geometry (Size, Position, Velocity, add2D, Scalar, Scalar2D) -- para el estado del juego

import Data.Maybe (fromMaybe, isNothing)

data MonigoteGameState = MonigoteGameState
  { 
    monigoteKeysPressed   :: Sets.Set Key, -- conjunto de teclas presionadas
    monigotePosition      :: Position,   -- metros
    monigoteVelocity      :: Velocity,   -- metros/segundo
    monigoteWindowSize    :: (Int, Int), -- tamaño de la ventana en píxeles
    monigoteInAir         :: Bool,      -- True si el monigote está en el aire (no en el suelo)
    monigoteHeight        :: Scalar,     -- metros
    monigoteWidth         :: Scalar,     -- metros
    monigoteAnimationWalk :: Float,      -- fase de caminata para animar las piernas
    monigoteOrientation :: Orientation
  }
  deriving (Show, Eq)

data Orientation = LeftFacing | RightFacing deriving (Show, Eq) --Simple enumerado para la orientación del monigote

-- Show personalizado para Set
showSet :: (Show a) => Sets.Set a -> String -- Muestra un Set como una lista entre llaves
showSet s = "{" ++ formatElements (Sets.toList s) ++ "}" -- Transforma el Set en lista y la formatea
  where
    formatElements []     = ""
    formatElements [x]    = show x
    formatElements (x:xs) = show x ++ ", " ++ formatElements xs -- Formatea recursivamente los elementos

-- Abstracciones sobre los estados que se implementan.
class KeysPressedState a where -- Cualquier tipo a que quiera comportarse como un estado con teclas presionadas debe implementar estas funciones
  keysPressed :: a -> Sets.Set Key
  addKey      :: a -> Key -> a
  deleteKey   :: a -> Key -> a

instance KeysPressedState MonigoteGameState where --Aquí le decimos a Haskell cómo aplicar la clase a nuestro tipo concreto MonigoteGameState
  keysPressed   = monigoteKeysPressed -- Devuelve el conjunto de teclas presionadas del estado
  addKey s k    = s { monigoteKeysPressed = Sets.insert k (monigoteKeysPressed s) } -- crea una copia del estado con la tecla k añadida al conjunto
  deleteKey s k = s { monigoteKeysPressed = Sets.delete k (monigoteKeysPressed s) } -- crea una copia del estado con la tecla k eliminada del conjunto

class WindowSizeState a where -- Cualquier tipo a que quiera comportarse como un estado con tamaño de ventana debe implementar estas funciones
  windowSize    :: a -> (Int, Int) -- Devuelve el tamaño de la ventana (ancho, alto)
  setWindowSize :: a -> (Int, Int) -> a -- Crea una copia del estado con el nuevo tamaño de ventana

instance WindowSizeState MonigoteGameState where --Aquí le decimos a Haskell cómo aplicar la clase a nuestro tipo concreto MonigoteGameState
  windowSize             = monigoteWindowSize -- Devuelve el tamaño de la ventana del estado
  setWindowSize state sz = state { monigoteWindowSize = sz } -- crea una copia del estado con el nuevo tamaño de ventana

-- Controlador de eventos con Maybe
type MaybeEventHandler a = Event -> a -> Maybe a -- Recibe un evento(Tecla , ratón) y un estado a por ejemplo MonigoteGameState y devuelve Maybe a (Nothing si no maneja el evento, Just nuevoEstado si lo maneja)

handleEvents :: [MaybeEventHandler gameState] -> Event -> gameState -> gameState -- Recibe una lista de manejadores de eventos, un evento y un estado, y devuelve el nuevo estado
handleEvents handlers event gs = fromMaybe gs result -- Si result es Nothing devuelve el estado original gs, si es Just nuevoEstado devuelve nuevoEstado
  where
    validHandlings = [ hd event gs | hd <- handlers, not (isNothing (hd event gs)) ] -- Aplica cada manejador de eventos al evento y al estado, y filtra los que no devuelven Nothing
    result         = if null validHandlings then Nothing else head validHandlings

tryHandleKeys :: (KeysPressedState kps) => Event -> kps -> Maybe kps -- Manejador de eventos para teclas
tryHandleKeys event state = -- Si el evento es una tecla presionada o liberada, actualiza el estado
  case event of
    EventKey k Down _ _ -> Just $ addKey state k -- Si la tecla se presiona, la añade al conjunto
    EventKey k Up   _ _ -> Just $ deleteKey state k
    _                   -> Nothing

tryHandleResizing :: (WindowSizeState wss) => Event -> wss -> Maybe wss -- Manejador de eventos para redimensionar ventana
tryHandleResizing event state =
  case event of
    EventResize size -> Just $ setWindowSize state size
    _                -> Nothing

-- Funciones Auxiliares
isKeyPressed :: MonigoteGameState -> Key -> Bool
isKeyPressed gs k = Sets.member k (keysPressed gs)

type Pixel = Float

meter2PixelFactor :: Pixel
meter2PixelFactor = 25

pixel2MeterFactor :: Scalar
pixel2MeterFactor = 1 / meter2PixelFactor

pixel2Meter :: Pixel -> Scalar
pixel2Meter px = px * pixel2MeterFactor

meter2Pixel :: Scalar -> Pixel
meter2Pixel m = m * meter2PixelFactor

-- Función principal del juego
monigoteGame :: IO ()
monigoteGame = play ventana white fps estadoInicial dibuja (handleEvents [tryHandleKeys, tryHandleResizing]) actualiza
  where
    fps = 60
    estadoInicial =
      MonigoteGameState
        { 
          monigoteKeysPressed   = Sets.empty,
          monigotePosition      = (0, 0),
          monigoteVelocity      = (0, 0),
          monigoteWindowSize    = (800, 600),
          monigoteInAir         = False,
          monigoteHeight        = 8,
          monigoteWidth         = 2,
          monigoteAnimationWalk = 0,
          monigoteOrientation = RightFacing
        }
    ventana     = InWindow "Juego del monigote " (windowSize estadoInicial) (100, 100)
    floorHeight = meter2Pixel 2

    dibuja :: MonigoteGameState -> Picture
    dibuja estado =
      Pictures [monigote, suelo, textoInformativo, textoPos, textoVel, textIsInAir, textAnimation]
      where
        (windowWidth, windowHeight) =
          let (w, h) = monigoteWindowSize estado
          in (fromIntegral w, fromIntegral h)

        floorY      = floorHeight - windowHeight / 2
        leftMarginX = meter2Pixel 1 - windowWidth / 2 :: Float

        textoInformativo =
          Translate leftMarginX (windowHeight / 2 - 50) $
            Scale 0.25 0.25 $
              Text $ "Teclas: " ++ showSet (keysPressed estado)

        textoPos =
          Translate leftMarginX (windowHeight / 2 - 90) $
            Scale 0.25 0.25 $
              Text $ "Posicion: " ++ show (monigotePosition estado)

        textoVel =
          Translate leftMarginX (windowHeight / 2 - 130) $
            Scale 0.25 0.25 $
              Text $ "Velocidad: " ++ show (monigoteVelocity estado)

        textIsInAir =
          Translate leftMarginX (windowHeight / 2 - 170) $
            Scale 0.25 0.25 $
              Text $ "IsInAir: " ++ show (monigoteInAir estado)

        textAnimation =
          Translate leftMarginX (windowHeight / 2 - 210) $
            Scale 0.25 0.25 $
              Text $ "Animation: " ++ show (monigoteAnimationWalk estado)

        suelo =
          Translate 0 (floorHeight / 2 - windowHeight / 2) $
            rectangleSolid windowWidth floorHeight

        monigote = drawMonigote estado

        -- monigotePosPoint = Translate (meter2Pixel x) (meter2Pixel y) $ ThickCircle 5 5 where (x, y) = monigotePosition estado

        -- === DIBUJO DEL MONIGOTE (cabeza + tronco + brazos + piernas) ===
        -- Origen en los "pies": (x,y) en metros es la base del personaje.
        drawMonigote :: MonigoteGameState -> Picture
        drawMonigote st =
          Translate px py $ -- Mueve a la posición (px, py) en píxeles
            Scale facing 1 $ -- Refleja en X si mira a la izquierda
              Pictures [backArm, backLeg, torso, frontArm, frontLeg, headPic] -- Se dibuja de izquierda a de derecha. Cuidado con las extremidades.
          where
            (x, y)  = monigotePosition st
            (vx, _) = monigoteVelocity st
            px      = meter2Pixel x
            py      = meter2Pixel y

            h = monigoteHeight st     -- altura total aprox (m)
            w = monigoteWidth st      -- “ancho base” (m)

            -- Conversión a píxeles para longitudes de cada parte
            headR  = meter2Pixel (h * 0.12)  -- radio cabeza
            torsoH = meter2Pixel (h * 0.42)  -- alto del tronco
            torsoW = meter2Pixel (w * 0.60)  -- ancho del tronco
            limbL  = meter2Pixel (h * 0.34)  -- longitud brazos/piernas
            limbW  = meter2Pixel (w * 0.30)  -- grosor brazos/piernas

            -- Alturas de anclaje (desde los pies)
            hipY      = limbL          -- cadera encima de los pies
            shoulderY = hipY + torsoH  -- hombros sobre la cadera

            -- Orientación: 1 → derecha; -1 → izquierda (según velocidad X)
            facing = if monigoteOrientation st == LeftFacing then (-1) else 1

            -- Balanceo (en grados): solo en suelo y si hay velocidad
            speedMag  = abs vx -- magnitud de la velocidad horizontal
            swingBase = if monigoteInAir st then 0 else 18 -- grados
            swing     = swingBase * sin (monigoteAnimationWalk st)

            -- Cabeza con ojos
            headPic =
              Translate 0 (shoulderY + headR) $
                let eyeR       = headR * 0.10
                    eyeOffsetX = headR * 0.35
                    eyeOffsetY = headR * 0.15
                in Pictures
                    [ 
                      Color (makeColorI 230 200 170 255) $ circleSolid headR,
                      Translate ( eyeOffsetX) eyeOffsetY $ Color black $ circleSolid eyeR
                    ]

            -- Tronco (rectángulo)
            torso =
              Translate 0 (hipY + torsoH / 2) $
                Color (makeColorI 60 60 60 255) $
                  rectangleSolid torsoW torsoH

            -- Piernas: ancladas en cadera, rotan ±swing
            leg theta =
              Translate 0 hipY $
                Rotate theta $
                  Color (makeColorI 40 40 255 255) $
                    Translate 0 (-limbL / 2) $
                      rectangleSolid limbW limbL
            frontLeg = leg swing
            backLeg = leg (-swing)

            -- Brazos: anclados en hombros, rotan en contrafase
            arm theta =
              Translate 0 shoulderY $
                Rotate theta $
                  Color (makeColorI 255 70 70 255) $
                    Translate 0 (-limbL / 2) $
                      rectangleSolid limbW limbL
            frontArm = arm (-swing)
            backArm = arm swing

    actualiza :: Float -> MonigoteGameState -> MonigoteGameState
    actualiza deltaTime currentState =
      applyPhisics $
        foldr
          (
            \(condition, effect) gameState ->
            if condition gameState then effect gameState else gameState
          )
          currentState
          conditions
      where
        -- deltaTime es el tiempo transcurrido (segundos) desde el último frame.
        conditions =
          [ ( \state -> isKeyPressed state (Char 'a') || isKeyPressed state (SpecialKey KeyLeft),
            goLeft),
            ( \state -> isKeyPressed state (Char 'd') || isKeyPressed state (SpecialKey KeyRight),
            goRight),
            ( \state -> isKeyPressed state (Char 'w') || isKeyPressed state (SpecialKey KeyUp),
            jump)
          ]

        jump :: MonigoteGameState -> MonigoteGameState
        jump state =
          if monigoteInAir state
            then state
            else state { 
                monigoteVelocity = (vx, 10),
                monigoteInAir = True 
              }
                where (vx, _) = monigoteVelocity state

        goRight :: MonigoteGameState -> MonigoteGameState
        goRight state = state { monigoteVelocity = (10, vy) }
          where (_, vy) = monigoteVelocity state

        goLeft :: MonigoteGameState -> MonigoteGameState
        goLeft state = state { monigoteVelocity = (-10, vy) }
          where (_, vy) = monigoteVelocity state

        applyPhisics :: MonigoteGameState -> MonigoteGameState
        applyPhisics state =
          state
            { 
              monigotePosition      = (newX, newY),
              monigoteInAir         = newY /= floorY,
              monigoteVelocity      = (newVX, newVY),
              monigoteAnimationWalk = newAnimationWalk,
              monigoteOrientation = newOrientation
            }
          where
            (oldX, oldY)   = monigotePosition state
            (oldVX, oldVY) = monigoteVelocity state

            (windowWidthM, windowHeightM) = ((pixel2Meter . fromIntegral) w, (pixel2Meter . fromIntegral) h)
              where (w, h) = monigoteWindowSize state

            floorY = (pixel2Meter floorHeight) - windowHeightM / 2

            newX = clampRange (oldX + oldVX * deltaTime) (-windowWidthM / 2 + marging, windowWidthM / 2 - marging)
              where marging = 2 -- m
            newY = clampRange (oldY + oldVY * deltaTime) (floorY, windowHeightM / 2)

            newVY =
              if newY > floorY
                then oldVY - 9.8 * deltaTime
                else 0

            candidateNewVX = oldVX * 0.01 ** deltaTime
            newVX =
              if abs candidateNewVX < 0.1
                then 0
                else candidateNewVX

            oldOrientation = monigoteOrientation state
            newOrientation
              | oldOrientation == LeftFacing && newVX > 0 = RightFacing
              | oldOrientation == RightFacing && newVX < 0 = LeftFacing
              | otherwise = oldOrientation
            -- Avance de fase de caminata
            moving            = abs newVX > 0.3
            freq              = 1 + (abs newVX) / 1.5
            newAnimationWalk =
              if not (monigoteInAir state) && moving
                then modF (2*pi) $ monigoteAnimationWalk state + deltaTime * realToFrac freq
                else
                  if monigoteAnimationWalk state > 1.5*pi -- Entre pi y 2 pi. Hay que acercarlo a 2 pi
                    then roundP $ monigoteAnimationWalk state * 2 ** deltaTime
                    else 
                      if monigoteAnimationWalk state > pi -- Entre pi y 2 pi. Hay que acercarlo a pi
                        then roundP $ monigoteAnimationWalk state * 0.5 ** deltaTime 
                        else
                          if monigoteAnimationWalk state > pi/2 -- Entre 0 y pi. Hay que acercalo a pi.
                            then roundP $ monigoteAnimationWalk state * 2 ** deltaTime
                            -- Hay que acercarlo a 0
                            else roundP $ monigoteAnimationWalk state * 0.1 ** deltaTime
            -- Si x es cercano a 0, pi o 2pi, devuelve 0 (estado nulo de animación).
            roundP x
              | x > 2*pi - err = 0
              | pi - err < x && x < pi + err = 0
              | x < err = 0
              | otherwise = x
                where err = pi/16

            -- "mod" para Float
            modF :: Float -> Float -> Float
            modF m x
              | result < 0 = result + m
              | otherwise  = result
                where
                  result = x - (m * (fromIntegral (floor (x / m)) :: Float))

            clampRange :: Scalar -> Scalar2D -> Scalar
            clampRange value (minV, maxV)
              | value > maxV = maxV
              | value < minV = minV
              | otherwise    = value

main :: IO ()
main = monigoteGame
