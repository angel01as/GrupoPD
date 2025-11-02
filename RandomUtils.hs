-- Módulo con utilidades para generar valores pseudo-aleatorios
--
-- ¿Qué es el algoritmo LCG (Generador Congruencial Lineal)?
--
-- Es una fórmula matemática simple: nuevoNumero = (a * semilla + c) mod m
-- donde a, c, y m son constantes cuidadosamente elegidas (1103515245, 12345, 100000).
-- Cada número generado sirve como semilla para el siguiente.


module RandomUtils (
  generatePositionFromSeed,
  generateAllRobotPositions,
  generateSafeRobotPosition,
  generateNonOverlappingObstacles
) where

import Entities (Obstacle(..), ObstacleType(..), ObstacleShape(..))
import Geometry (Size, Position, add2D)
import Robot (Robot(..))
import Data.List (find)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

-- Genera una posición aleatoria basándose en una semilla 
-- Esta función NO usa IO, por lo que puede usarse tanto al inicio como al resetear
-- 
-- Parámetros:
--   (maxX, maxY): Límites del área de juego (mitad del ancho y alto del escenario)
--                 Por ejemplo, si el escenario es 100x70, los límites serían (50, 35)
--   seedBase:     Semilla para generar aleatoriedad (puede ser tiempo del sistema o tiempo de juego)
--   robotID:      ID del robot (para que cada robot tenga una posición diferente con la misma semilla)
--
-- Retorna: Una tupla (x, y) con la posición aleatoria dentro del área de juego
generatePositionFromSeed :: (Float, Float) -> Double -> Int -> (Float, Float)
generatePositionFromSeed (maxX, maxY) seedBase robotID = (x * 0.8, y * 0.8)  -- Multiplicamos por 0.8 para usar solo 80% del área (evita spawns cerca de bordes)
  where
    -- Convertir la semilla Float a un Int grande (multiplicamos por 1 millón para tener más precisión)
    seedInt = round (seedBase * 1000000) :: Int
    
    -- Combinar la semilla con el ID del robot (7919 es un número primo para mejor distribución)
    -- Esto asegura que cada robot obtenga una posición diferente con la misma semilla base
    combinedSeed = seedInt + robotID * 7919
    
    -- Generador Congruencial Lineal (LCG) para generar coordenada X
    -- Los números 1103515245 y 12345 son constantes estándar del algoritmo LCG
    randX = fromIntegral ((combinedSeed * 1103515245 + 12345) `mod` 100000) / 100000  -- randX está entre 0.0 y 1.0
    x = randX * maxX * 2 - maxX  -- Convertimos randX (0-1) a un rango entre -maxX y +maxX
    
    -- Generador Congruencial Lineal (LCG) para coordenada Y
    -- Usamos +31337 para que Y tenga una secuencia diferente a X (evita patrones diagonales)
    randY = fromIntegral (((combinedSeed + 31337) * 1103515245 + 12345) `mod` 100000) / 100000  -- randY está entre 0.0 y 1.0
    y = randY * maxY * 2 - maxY  -- Convertimos randY (0-1) a un rango entre -maxY y +maxY

-- Genera posiciones para una lista de IDs de robots (función auxiliar)
-- 
-- Parámetros:
--   bounds:    Límites del área de juego (mitad del ancho, mitad del alto)
--   seedBase:  Semilla base para generar aleatoriedad
--   robotIDs:  Lista de IDs de robots para los que generar posiciones
--
-- Retorna: Lista de tuplas (ID, (x, y)) con las posiciones generadas para cada robot

generateAllRobotPositions :: (Float, Float) -> Double -> [Int] -> [(Int, (Float, Float))]
generateAllRobotPositions bounds seedBase robotIDs = 
  [(robotID, generatePositionFromSeed bounds seedBase robotID) | robotID <- robotIDs]

-- Genera una posición "segura" para un robot evitando solaparse con obstáculos.
-- Prueba hasta 1001 offsets deterministas alrededor de la semilla base.
-- Usa un AABB simple con tamaño fijo de robot (5x5) para la comprobación.
generateSafeRobotPosition :: (Float, Float) -> Double -> Int -> [Obstacle] -> (Float, Float)
generateSafeRobotPosition bounds seedBase robotID obstacles =
  let candidates = [ generatePositionFromSeed bounds (seedBase + fromIntegral offset * 0.01) robotID | offset <- [0..2000] ]
      ok p = not (any (collidesWithObstacle p) obstacles)
  in fromMaybe (generatePositionFromSeed bounds seedBase robotID) (find ok candidates)
  where
    collidesWithObstacle :: (Float, Float) -> Obstacle -> Bool
    collidesWithObstacle (x,y) o =
      let (ox,oy) = obstaclePosition o
          (rw, rh) = (6, 6) -- tamaño del robot actualizado a (6, 6)
          (ow, oh) = obstacleSize o
      in abs (x - ox) < (rw + ow) / 2 && abs (y - oy) < (rh + oh) / 2

-- Verificación AABB simple
overlaps :: (Float,Float) -> (Float,Float) -> (Float,Float) -> (Float,Float) -> Bool
overlaps (x1,y1) (sx1,sy1) (x2,y2) (sx2,sy2) =
  abs (x1 - x2) < (sx1 + sx2)/2 && abs (y1 - y2) < (sy1 + sy2)/2

-- Genera obstáculos aleatorios evitando solapar robots y otros obstáculos ya colocados.
-- Intenta recolocar hasta 500 veces por obstáculo; si no cabe, lo omite.
generateNonOverlappingObstacles :: Size -> Float -> [Robot] -> [Obstacle]
generateNonOverlappingObstacles (w,h) seed robots = take 4 $ go [] ids
  where
    bounds = (w/2, h/2)
    ids = [1001..]
    -- Probabilidad por tipo tal como en Game.generateRandomObstacles
    pickType rid = let p = frac (sin (seed*0.73 + fromIntegral rid*12.3) * 43758.5453)
                   in if p < 0.05 then Solid else if p < 0.20 then Hazard else if p < 0.40 then Bomb else Special
    frac x = x - fromIntegral (floor x :: Int)

    go acc (rid:rest)
      | length acc >= 4 = acc
      | otherwise =
          case tryPlace acc rid 0 of
            Just o  -> go (acc ++ [o]) rest
            Nothing -> go acc rest

    tryPlace acc rid k
      | k > 500 = Nothing
      | otherwise =
          let pos = generatePositionFromSeed bounds (realToFrac seed + fromIntegral k * 0.03) rid
              t = pickType rid
              (shape, sz, col) = case t of
                Solid  -> (Square, (8,8), 0)
                Hazard -> (Circle, (6,6), 0)
                Bomb   -> (Square, (4,4), 0)
                Special-> (Polygon (regularPolygonVerts 8 3.0), (6,6), 0)
              localVerts = case shape of
                Square -> let (sx, sy) = sz in [(-sx/2,-sy/2),(sx/2,-sy/2),(sx/2,sy/2),(-sx/2,sy/2)]
                Circle -> circleApproxVerts (fst sz / 2) 16
                Polygon pts -> pts
              worldVerts = map (add2D pos) localVerts
              candidate = Obs { obstacleID = rid
                               , obstacleType = t
                               , obstacleShape = shape
                               , obstaclePosition = pos
                               , obstacleVertices = worldVerts
                               , obstacleSize = sz
                               , obstacleOrientation = 0
                               , obstacleHealth = 100
                               , obstacleTimer = Nothing
                               , obstacleColor = undefined -- color no usado aquí; lo decide Rendering
                               }
          in if collidesAny pos sz robots acc
               then tryPlace acc rid (k+1)
               else Just candidate

    -- Chequear contra robots y obstáculos ya aceptados
    collidesAny (x,y) sz robots' obstacles' =
      let robotOverlap = any (\r -> overlaps (x,y) sz (robotPosition r) (robotSize r)) robots'
          obsOverlap   = any (\o -> overlaps (x,y) sz (obstaclePosition o) (obstacleSize o)) obstacles'
      in robotOverlap || obsOverlap

    circleApproxVerts r n = [ (r * cos (theta i), r * sin (theta i)) | i <- [0..n-1] ]
      where theta i = 2*pi*fromIntegral i / fromIntegral n
    regularPolygonVerts n r = [ (r * cos (theta i), r * sin (theta i)) | i <- [0..n-1] ]
      where theta i = 2*pi*fromIntegral i / fromIntegral n

