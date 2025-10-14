-- Tarea1Gloss.hs
-- Tres demos sencillas de Gloss en un solo programa:
-- 1) Dibujo fijo + transformaciones
-- 2) Animación básica (depende del tiempo)
-- 3) Interacción con teclado (mover un cuadrado)
-- Cambia de demo con las teclas '1', '2', '3'. Mueve el ratón para ver la mira.

{-# OPTIONS_GHC -Wall #-}
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- ====== Modelo ======
data Modo
  = DemoDisplay
  | DemoAnimate
  | DemoPlay
  deriving (Eq, Show)

data Mundo = Mundo
  { modo     :: Modo
  , tiempo   :: Float      -- para la demo de animación
  , posX     :: Float      -- para la demo de interacción (mover en X)
  , velX     :: Float
  , mouseXY  :: (Float,Float) -- para mostrar una mira con el ratón (eventos de ratón)
  } deriving Show

inicio :: Mundo
inicio = Mundo
  { modo    = DemoDisplay
  , tiempo  = 0
  , posX    = 0
  , velX    = 0
  , mouseXY = (0,0)
  }

-- ====== Ventana / parámetros ======
win :: Display
win = InWindow "Tarea 1 - Gloss (1:Display  2:Animate  3:Play)" (800, 600) (100, 100)

fondo :: Color
fondo = white

fps :: Int
fps = 60

-- ====== Dibujo ======
dibuja :: Mundo -> Picture
dibuja w = Pictures
  [ case modo w of
      DemoDisplay -> escenaDisplay
      DemoAnimate -> escenaAnimate (tiempo w)
      DemoPlay    -> escenaPlay (posX w)
  , uiComun (modo w)
  , mira (mouseXY w)
  ]

-- Texto de ayuda común
uiComun :: Modo -> Picture
uiComun m =
  Translate (-390) 270 $
    Scale 0.1 0.1 $
      Color black $
        Text $ "Modo: " ++ show m ++ "   (1=Display, 2=Animate, 3=Play)"

-- Mira que sigue al raton (esto es para demostrar eventos de raton)
mira :: (Float,Float) -> Picture
mira (mx,my) =
  Pictures
    [ Color (withAlpha 0.4 black) $ Translate mx my $ Pictures
        [ Line [(-10,0),(10,0)]
        , Line [(0,-10),(0,10)]
        , circle 12
        ]
    ]

-- ====== ESCENA 1: Display (dibujo fijo + transformaciones) ======
escenaDisplay :: Picture
escenaDisplay =
  Pictures
    [ Color (greyN 0.85) $ rectangleWire 760 560
    , Translate (-220) 90 $ Color red $ circleSolid 60
    , Translate (220) 90  $ Color blue $ rectangleSolid 120 80
    , Translate 0 (-80) $ Rotate 25 $ Scale 1.2 1.2 $
        Color (makeColorI 0 160 0 255) $
        polygon [(-60,-40), (60,-40), (0,60)]
    , Translate (-380) (-270) $ Scale 0.1 0.1 $ Color black $
        Text "Display: Picture + Color + Translate/Rotate/Scale"
    ]

-- ====== ESCENA 2: Animate (depende del tiempo) ======
escenaAnimate :: Float -> Picture
escenaAnimate t =
  let ang = t * 45               -- grados/seg aprox.
  in Pictures
      [ Color (greyN 0.93) $ rectangleWire 760 560
      , Translate 0 0
        $ Color (withAlpha 0.9 orange)
        $ Rotate ang
        $ polygon [(-100,-60),(100,-60),(130,0),(100,60),(-100,60),(-130,0)]
      , Translate (-380) (-270) $ Scale 0.1 0.1 $ Color black $
          Text "Animate: dibuja t -> Picture (rotando una figura)"
      ]

-- ====== ESCENA 3: Play (interacción con teclado) ======
escenaPlay :: Float -> Picture
escenaPlay x =
  Pictures
    [ Color (greyN 0.9) $ rectangleWire 760 560
    , Translate x (-50) $ Color violet $ rectangleSolid 60 60
    , Translate (-360) 260 $ Scale 0.1 0.1 $ Color black $
        Text "Play: A/← izquierda, D/→ derecha   (borde a ±380)"
    ]

-- ====== Eventos ======
manejaEvento :: Event -> Mundo -> Mundo
-- Cambiar de demo con 1/2/3
manejaEvento (EventKey (Char '1') Down _ _) w = w { modo = DemoDisplay }
manejaEvento (EventKey (Char '2') Down _ _) w = w { modo = DemoAnimate }
manejaEvento (EventKey (Char '3') Down _ _) w = w { modo = DemoPlay    }

-- Teclado para demo Play (A/D o flechas)
manejaEvento (EventKey (SpecialKey KeyLeft)  Down _ _) w = w { velX = -200 }
manejaEvento (EventKey (SpecialKey KeyRight) Down _ _) w = w { velX =  200 }
manejaEvento (EventKey (Char 'a')           Down _ _) w = w { velX = -200 }
manejaEvento (EventKey (Char 'd')           Down _ _) w = w { velX =  200 }

-- Al soltar, paramos si corresponde
manejaEvento (EventKey (SpecialKey KeyLeft)  Up _ _) w
  | velX w < 0  = w { velX = 0 }
  | otherwise   = w
manejaEvento (EventKey (SpecialKey KeyRight) Up _ _) w
  | velX w > 0  = w { velX = 0 }
  | otherwise   = w
manejaEvento (EventKey (Char 'a') Up _ _) w
  | velX w < 0  = w { velX = 0 }
  | otherwise   = w
manejaEvento (EventKey (Char 'd') Up _ _) w
  | velX w > 0  = w { velX = 0 }
  | otherwise   = w

-- Evento de movimiento del ratón: actualizamos la mira
manejaEvento (EventMotion (mx,my)) w = w { mouseXY = (mx,my) }

-- Otros eventos no cambian el estado
manejaEvento _ w = w

-- ====== Actualización (dt en segundos) ======
actualiza :: Float -> Mundo -> Mundo
actualiza dt w =
  case modo w of
    DemoDisplay -> w  -- sin cambios con el tiempo
    DemoAnimate -> w { tiempo = tiempo w + dt }
    DemoPlay    ->
      let x' = posX w + velX w * dt
          halfW = 380
          xClamped
            | x' < -halfW = -halfW
            | x' >  halfW =  halfW
            | otherwise   =  x'
      in w { posX = xClamped }

-- ====== Main ======
main :: IO ()
main = play win fondo fps inicio dibuja manejaEvento actualiza