{-# OPTIONS_GHC -Wall #-} -- Advertencias adicionales

module Geometry where

-- Escala base (Float) para todo
type Scalar   = Double
type Scalar2D = (Scalar, Scalar)
type Point    = Scalar2D   -- Un punto 2D en el espacio. SR: {(0,0);(1,0),(0,1)}
type Vector   = Scalar2D   -- Vector siempre se considera que empieza en (0,0)
type Velocity = Vector
type Angle    = Scalar           -- Un ángulo con decimales. Puede ser grados o radianes, positivo y negativo.
type Distance = Scalar             -- Un valor de distancia con decimales.
type Position = Point             -- Representa la posición de objeto en un mundo 2D.
type Size     = (Scalar, Scalar)  -- Ancho y alto de un objeto (width, height)

-- Calcula la distancia euclídea entre dos posiciones en el espacio. Toma dos puntos como entrada y devuelve la distancia lineal que los separa.

distanceBetween :: Position -> Position -> Distance
distanceBetween (x1, y1) (x2, y2) = sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)

-- Determina el ángulo desde una posición origen hacia una posición objetivo. Útil para calcular la dirección en la que debe apuntar o moverse un objeto. (radianes, rango [-pi, pi])
angleToTarget :: Position -> Position -> Angle
angleToTarget (x1, y1) (x2, y2) = atan2 (y2 - y1) (x2 - x1)

-- Convierte un ángulo expresado en grados a su equivalente en radianes.
deg2rad :: Angle -> Angle
deg2rad a = a * pi / 180

-- Convierte un ángulo expresado en radianes a su equivalente en grados.
rad2deg :: Angle -> Angle
rad2deg a = a * 180 / pi

-- Realiza la resta de dos vectores, devolviendo un nuevo vector que representa la diferencia entre ellos.
subVec :: Vector -> Vector -> Vector
subVec (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

-- Genera una lista de vértices (puntos) a partir de puntos base y un ángulo de rotación.
rotateVertices :: [Point] -> Angle -> [Point]
rotateVertices ps theta = map (rotarCentro theta centro) ps
  where
    centro :: Point
    centro = prodByScalar (1/numPts) (foldr1 add2D ps)
      where
        numPts = fromIntegral $ length ps

    rotarCentro :: Angle -> Point -> Point -> Point
    rotarCentro t (cx, cy) (x, y) =
      let dx = x - cx
          dy = y - cy
          c  = cos t
          s  = sin t
          x' = dx * c - dy * s
          y' = dx * s + dy * c
      in (x' + cx, y' + cy)

-- Aplica una traslación a una lista de vértices (puntos)
translateVertices :: [Point] -> Vector -> [Point]
translateVertices ps vec = map (add2D vec) ps

-- Calcula el producto escalar (dot product) entre dos puntos tratados como vectores
dot :: Vector -> Vector -> Scalar
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

-- Resta un punto de otro, devolviendo un nuevo punto que representa la diferencia entre las coordenadas.
sub :: Point -> Point -> Point
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

-- Calcula el vector perpendicular a un punto dado (tratado como vector).
perp :: Vector -> Vector
perp (x, y) = (-y, x)

-- Verifica si un punto se encuentra dentro de los límites definidos por un tamaño dado centrado en (0,0)
isInBounds :: Point -> Size -> Bool
isInBounds (x, y) (width, height) = left && top && right && bottom
    where
        left = x >= -width/2
        top = y <= height/2
        right = x <= width/2
        bottom = y >= -height/2

--  mul: tal que (w,h) `mul` (sw,sh) = (w * sw, h * sh)
mul :: Scalar2D -> Scalar2D -> Scalar2D
mul (w,h) (sw,sh) = (w * sw, h * sh)

-- Funciones auxiliares adicionales.

-- Suma dos Scalar2D
add2D :: Scalar2D -> Scalar2D -> Scalar2D
add2D (px, py) (vx, vy) = (px + vx, py + vy)

-- Resta dos Scalar2D
sub2D :: Scalar2D -> Scalar2D -> Scalar2D
sub2D (px, py) (vx, vy) = (px - vx, py - vy)

-- Realiza el producto por escalar sobre un Vector.
prodByScalar :: Scalar -> Vector -> Vector
prodByScalar t (vx, vy) = (t * vx, t * vy)

-- Da como Scalar2D el factor de proyección sobre el ángulo dado.
angleFactor :: Angle -> Scalar2D
angleFactor a = (cos a, sin a)