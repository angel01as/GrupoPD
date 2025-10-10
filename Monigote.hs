module Monigote where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Set as Sets hiding (Set (show))
import Geometry (Size, Position, Velocity, add2D, Scalar, Scalar2D)

import Data.Maybe (fromMaybe, isNothing)

data MonigoteGameState = MonigoteGameState
  { monigoteKeysPressed   :: Sets.Set Key
  , monigotePosition      :: Position   -- m
  , monigoteVelocity      :: Velocity   -- m/s
  , monigoteWindowSize    :: (Int, Int)
  , monigoteInAir         :: Bool
  , monigoteHeight        :: Scalar     -- m
  , monigoteWidth         :: Scalar     -- m
  , monigoteAnimationWalk :: Float      -- fase de caminata para animar las piernas
  }
  deriving (Show, Eq)

-- Show personalizado para Set
showSet :: (Show a) => Sets.Set a -> String
showSet s = "{" ++ formatElements (Sets.toList s) ++ "}"
  where
    formatElements []     = ""
    formatElements [x]    = show x
    formatElements (x:xs) = show x ++ ", " ++ formatElements xs

-- Abstracciones sobre los estados que se implementan.
class KeysPressedState a where
  keysPressed :: a -> Sets.Set Key
  addKey      :: a -> Key -> a
  deleteKey   :: a -> Key -> a

instance KeysPressedState MonigoteGameState where
  keysPressed   = monigoteKeysPressed
  addKey s k    = s { monigoteKeysPressed = Sets.insert k (monigoteKeysPressed s) }
  deleteKey s k = s { monigoteKeysPressed = Sets.delete k (monigoteKeysPressed s) }

class WindowSizeState a where
  windowSize    :: a -> (Int, Int)
  setWindowSize :: a -> (Int, Int) -> a

instance WindowSizeState MonigoteGameState where
  windowSize             = monigoteWindowSize
  setWindowSize state sz = state { monigoteWindowSize = sz }

-- Controlador de eventos con Maybe
type MaybeEventHandler a = Event -> a -> Maybe a

handleEvents :: [MaybeEventHandler gameState] -> Event -> gameState -> gameState
handleEvents handlers event gs = fromMaybe gs result
  where
    validHandlings = [ hd event gs | hd <- handlers, not (isNothing (hd event gs)) ]
    result         = if null validHandlings then Nothing else head validHandlings

tryHandleKeys :: (KeysPressedState kps) => Event -> kps -> Maybe kps
tryHandleKeys event state =
  case event of
    EventKey k Down _ _ -> Just $ addKey state k
    EventKey k Up   _ _ -> Just $ deleteKey state k
    _                   -> Nothing

tryHandleResizing :: (WindowSizeState wss) => Event -> wss -> Maybe wss
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
        { monigoteKeysPressed   = Sets.empty
        , monigotePosition      = (0, 0)
        , monigoteVelocity      = (0, 0)
        , monigoteWindowSize    = (800, 600)
        , monigoteInAir         = False
        , monigoteHeight        = 12
        , monigoteWidth         = 2
        , monigoteAnimationWalk = 0
        }
    ventana     = InWindow "Juego del monigote" (windowSize estadoInicial) (100, 100)
    floorHeight = meter2Pixel 2

    dibuja :: MonigoteGameState -> Picture
    dibuja estado =
      Pictures [monigote, suelo, textoInformativo, textoPos, textoVel, textIsInAir, monigotePosPoint]
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

        suelo =
          Translate 0 (floorHeight / 2 - windowHeight / 2) $
            rectangleSolid windowWidth floorHeight

        monigote = drawMonigote estado

        monigotePosPoint =
          let (x, y) = monigotePosition estado
          in Translate (meter2Pixel x) (meter2Pixel y) $
              ThickCircle 5 5

        -- === DIBUJO DEL MONIGOTE (cabeza + tronco + brazos + piernas) ===
        -- Origen en los "pies": (x,y) en metros es la base del personaje.
        drawMonigote :: MonigoteGameState -> Picture
        drawMonigote st =
          Translate px py $
            Scale facing 1 $
              Pictures [legs, torso, arms, headPic]
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
            facing = if vx < -0.1 then (-1) else 1

            -- Balanceo (en grados): solo en suelo y si hay velocidad
            speedMag  = abs vx
            swingBase = if monigoteInAir st then 0 else 18
            swing     = swingBase * sin (monigoteAnimationWalk st * (3 + min 2 (realToFrac speedMag)))

            -- Cabeza con ojos
            headPic =
              Translate 0 (shoulderY + headR + 2) $
                let eyeR       = headR * 0.10
                    eyeOffsetX = headR * 0.45
                    eyeOffsetY = headR * 0.15
                in Pictures
                    [ Color (makeColorI 230 200 170 255) $ circleSolid headR
                    , Translate (-eyeOffsetX) eyeOffsetY $ Color black $ circleSolid eyeR
                    , Translate (  eyeOffsetX) eyeOffsetY $ Color black $ circleSolid eyeR
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
                  Color (makeColorI 40 40 40 255) $
                    Translate 0 (-limbL / 2) $
                      rectangleSolid limbW limbL

            legs = Pictures [ leg swing, leg (-swing) ]

            -- Brazos: anclados en hombros, rotan en contrafase
            arm theta =
              Translate 0 shoulderY $
                Rotate theta $
                  Color (makeColorI 70 70 70 255) $
                    Translate 0 (-limbL / 2) $
                      rectangleSolid limbW limbL

            arms = Pictures [ arm (-swing), arm swing ]

    actualiza :: Float -> MonigoteGameState -> MonigoteGameState
    actualiza deltaTime currentState =
      applyPhisics $
        foldr
          (\(condition, effect) gameState ->
            if condition gameState then effect gameState else gameState
          )
          currentState
          conditions
      where
        -- deltaTime es el tiempo transcurrido (segundos) desde el último frame.
        conditions =
          [ ( \state -> isKeyPressed state (Char 'a') || isKeyPressed state (SpecialKey KeyLeft)
            , goLeft
            )
          , ( \state -> isKeyPressed state (Char 'd') || isKeyPressed state (SpecialKey KeyRight)
            , goRight
            )
          , ( \state -> isKeyPressed state (Char 'w') || isKeyPressed state (SpecialKey KeyUp)
            , jump
            )
          ]

        jump :: MonigoteGameState -> MonigoteGameState
        jump state =
          if monigoteInAir state
            then state
            else state { monigoteVelocity = (0, 10), monigoteInAir = True }

        goRight :: MonigoteGameState -> MonigoteGameState
        goRight state = state { monigoteVelocity = (10, vy) }
          where (_, vy) = monigoteVelocity state

        goLeft :: MonigoteGameState -> MonigoteGameState
        goLeft state = state { monigoteVelocity = (-10, vy) }
          where (_, vy) = monigoteVelocity state

        applyPhisics :: MonigoteGameState -> MonigoteGameState
        applyPhisics state =
          state
            { monigotePosition      = (newX, newY)
            , monigoteInAir         = newY /= floorY
            , monigoteVelocity      = (newVX, newVY)
            , monigoteAnimationWalk = newAnimationWalk
            }
          where
            (oldX, oldY)   = monigotePosition state
            (oldVX, oldVY) = monigoteVelocity state

            (windowWidthM, windowHeightM) =
              let (w, h) = monigoteWindowSize state
              in ((pixel2Meter . fromIntegral) w, (pixel2Meter . fromIntegral) h)

            floorY = (pixel2Meter floorHeight) - windowHeightM / 2

            newX = clampRange (oldX + oldVX * deltaTime) (-windowWidthM / 2, windowWidthM / 2)
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

            -- Avance de fase de caminata
            moving            = abs newVX > 0.3
            freq              = 3 + min 2 (abs newVX)
            newAnimationWalk =
              if not (monigoteInAir state) && moving
                then monigoteAnimationWalk state + deltaTime * realToFrac freq
                else monigoteAnimationWalk state

            clampRange :: Scalar -> Scalar2D -> Scalar
            clampRange value (minV, maxV)
              | value > maxV = maxV
              | value < minV = minV
              | otherwise    = value

main :: IO ()
main = monigoteGame
