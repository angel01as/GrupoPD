module UIButton where

import Geometry
import WindowSizeState

data UIButton state = UIButton
  {
    buttonPosition :: Position, -- Posición y tamaños relativos al tamaño de la ventana. Entre -1 y 1
    buttonSize :: Size, -- Entre 0 y 2
    buttonText :: String,
    buttonHandler :: state -> state
  }

instance Show (UIButton state) where
  show (UIButton pos size _ _) = "UIButton { position = " ++ show pos ++ ", size = " ++ show size ++ " }"

instance Eq (UIButton state) where
  (UIButton pos1 size1 _ _) == (UIButton pos2 size2 _ _) = pos1 == pos2 && size1 == size2

class (WindowSizeState a) => MouseButtonState a where
  buttons :: a -> [UIButton a]
  handleLeftClick :: a -> Position -> a
  handleLeftClick oldState pos = foldr (\button state -> if isInside pos button then (buttonHandler button) state else state) oldState (buttons oldState)
    where
      isInside :: Position -> UIButton a -> Bool
      isInside (x, y) button = x >= bx - bsx / 2 && x <= bx + bsx / 2 && y >= by - bsy / 2 && y <= by + bsy / 2
        where 
          windowSize' = windowSize oldState
          (windowWidth, windowHeight) = (fromIntegral (fst windowSize'), fromIntegral (snd windowSize'))
          (bx, by) = (windowWidth / 2 * bx', windowHeight / 2 * by')
          (bsx, bsy) = (windowWidth / 2 * bsx', windowHeight / 2 * bsy')
          (bx', by') = buttonPosition button
          (bsx', bsy') = buttonSize button