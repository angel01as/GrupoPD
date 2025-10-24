module WindowSizeState where

class WindowSizeState a where -- Cualquier tipo a que quiera comportarse como un estado con tamaño de ventana debe implementar estas funciones
  windowSize    :: a -> (Int, Int) -- Devuelve el tamaño de la ventana (ancho, alto)
  setWindowSize :: a -> (Int, Int) -> a -- Crea una copia del estado con el nuevo tamaño de ventana
