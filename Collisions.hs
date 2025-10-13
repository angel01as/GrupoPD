{-# OPTIONS_GHC -Wall #-}

module Collisions
  ( checkCollision
  , detectRobotProjectileCollisions
  , detectRobotRobotCollisions
  , checkCollisions
  , RobotProjectileCollisionEvent
  , RobotRobotCollisionEvent
  ) where

import Geometry
  (Scalar2D, Point, Vector
  , dot, sub, perp )

import Entities (Projectile(..), GameEntity (vertices))
import Robot (Robot(..))
import Data.Maybe

-- Eventos de colisión con type
type RobotProjectileCollisionEvent = (Projectile, Robot)
type RobotRobotCollisionEvent      = (Robot, Robot)



-- Alg. SAT
-- Tomar vertices de un polígono y crear vectores de sus lados
-- Tomar las perpendiculares de esos vectores
-- Para cada perpendicular calcular la proyección de los vectores de los puntos en estas
-- Comprobar el cruce de los rangos del minimo al maximo de cada poligono, si en uno de estos NO hay cruce, no colisionan

-- checkCollision: Comprueba si dos rectángulos han colisionado utilizando el algoritmo apropiado.
-- all id significa que todos los elementos de la lista son True
checkCollision :: [Point] -> [Point] -> Bool
checkCollision [] _ = error "El primer polígono no debe estar vacío"
checkCollision _ [] = error "El segundo polígono no debe estar vacío"
checkCollision ra rb = and [ hayInterseccion a b | (a,b) <- zip rangosA rangosB ]
    where
        vPerps  = obtenVPerp ra ++ obtenVPerp rb
        rangosA = [rangoProyectado ra v | v <- vPerps]
        rangosB = [rangoProyectado rb v | v <- vPerps]


-- Calcula el vector perpendicular a las aristas del polígono definido por [Point]
obtenVPerp :: [Point] -> [Vector]
obtenVPerp ps = [perp (sub p2 p1) | (p1, p2) <- zip ps (tail ps ++ [head ps])]

-- Comprueba si dos rangos intersecan. Hacer any sobre todos ellos para ver si hay huecos.
hayInterseccion :: Scalar2D -> Scalar2D -> Bool
hayInterseccion (amin, amax) (bmin, bmax) = not (amax < bmin || bmax < amin)

-- Consigue el rango proyectado por un polígono sobre un vector
rangoProyectado :: [Point] -> Vector -> Scalar2D
rangoProyectado p v = (minimum proyecciones, maximum proyecciones)
    where proyecciones = [dot px v | px <- p]

-- detectRobotProjectileCollisions: Verifica qué proyectiles han colisionado con algún agente. Cuando detecte una colisión, debe generar el evento de colisión correspondiente.
-- Detección de colisiones Proyectil–Robot:
-- Devuelve una lista de tuplas (Projectile, Robot) que colisionan.
detectRobotProjectileCollisions :: [Projectile] -> [Robot] -> [RobotProjectileCollisionEvent]
detectRobotProjectileCollisions ps rs = catMaybes $ colisionRP <$> ps <*> rs
  where colisionRP p r = if checkCollision (vertices p) (vertices r)
                        then Just (p, r)
                        else Nothing
-- detectRobotProjectileCollisions ps rs =
--   [ (p, r)
--   | p <- ps
--   , r <- rs
--   , checkCollision (projectileVertices p) (robotVertices r)
--   ]

-- Detección de colisiones Robot–Robot:
-- Empareja robots una sola vez (i < j) y devuelve las tuplas (Robot, Robot) que colisionan.
detectRobotRobotCollisions :: [Robot] -> [RobotRobotCollisionEvent]
detectRobotRobotCollisions []     = []
detectRobotRobotCollisions rs = catMaybes $ colisionRobot <$> rs <*> rs
  where colisionRobot ra rb = if ra /= rb && checkCollision (vertices ra) (vertices rb)
                              then Just (ra,rb)
                              else Nothing
-- detectRobotRobotCollisions (r:rs) =
--   colisionesConR r rs ++ detectRobotRobotCollisions rs
--   where
--     -- Compara el robot r con cada robot del resto de la lista
--     colisionesConR :: Robot -> [Robot] -> [RobotRobotCollisionEvent]
--     colisionesConR _ [] = []
--     colisionesConR a (b:bs)
--       | checkCollision (robotVertices a) (robotVertices b) = (a, b) : colisionesConR a bs
--       | otherwise                                          = colisionesConR a bs

-- checkCollisions: Función principal que coordina todas las comprobaciones de colisión.
checkCollisions :: [Robot] -> [Projectile] -> ([RobotProjectileCollisionEvent], [RobotRobotCollisionEvent])
checkCollisions rs ps = (detectRobotProjectileCollisions ps rs, detectRobotRobotCollisions rs)