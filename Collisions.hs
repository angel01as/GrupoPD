{-# OPTIONS_GHC -Wall #-} -- Advertencias adicionales

module Collisions (
    checkCollisions
) where

import Geometry
import Entities
import Robot

data RobotProjectileCollisionEvent = RobProjColl
    {
        -- Temporal
        robotInProjColl :: Robot
    } deriving (Show, Eq)

data RobotRobotCollisionEvent = RobRobColl
    {
        -- Temporal
        robotInColl1 :: Robot
    } deriving (Show, Eq)



-- Alg. SAT
-- Tomar vertices de un polígono y crear vectores de sus lados
-- Tomar las perpendiculares de esos vectores
-- Para cada perpendicular calcular la proyección de los vectores de los puntos en estas
-- Comprobar el cruce de los rangos del minimo al maximo de cada poligono, si en uno de estos NO hay cruce, no colisionan

-- checkCollision: Comprueba si dos rectángulos han colisionado utilizando el algoritmo apropiado.
checkCollision :: [Point] -> [Point] -> Bool
checkCollision ra rb = any (==False) [hayInterseccion rangoA rangoB | (rangoA, rangoB) <- zip rangosPorVertA rangosPorVertB]
    where
        vPerps = obtenVPerp ra ++ obtenVPerp rb
        rangosPorVertA = [rangoProyectado ra v | v <- vPerps]
        rangosPorVertB = [rangoProyectado rb v | v <- vPerps]

-- Calcula el vector perpendicular a las aristas del polígono definido por [Point]
obtenVPerp :: [Point] -> [Vector]
obtenVPerp ps = [perp (sub p2 p1) | (p1, p2) <- zip ps (tail ps ++ [head ps])]

-- Comprueba si dos rangos intersecan. Hacer any sobre todos ellos para ver si hay huecos.
hayInterseccion :: Scalar2D -> Scalar2D -> Bool
hayInterseccion (amin, amax) (bmin, bmax) = firstCase || secondCase
    where
        firstCase = amin < bmax && amin > bmin
        secondCase = bmin < amax && bmin > amin
    
-- Consigue el rango proyectado por un polígono sobre un vector
rangoProyectado :: [Point] -> Vector -> Scalar2D
rangoProyectado p v = (minimum proyecciones, maximum proyecciones)
    where proyecciones = [dot px v | px <- p]

-- detectRobotProjectileCollisions: Verifica qué proyectiles han colisionado con algún agente. Cuando detecte una colisión, debe generar el evento de colisión correspondiente.
detectRobotProjectileCollisions :: [Projectile] -> [Robot] -> [RobotProjectileCollisionEvent]
detectRobotProjectileCollisions ps rs = []

-- detectRobotRobotCollisions: Comprueba y detecta las colisiones entre los diferentes robots del juego. Deberá generar el evento de colisión correspondiente.
detectRobotRobotCollisions :: [Robot] -> [RobotRobotCollisionEvent]
detectRobotRobotCollisions rs = []

-- checkCollisions: Función principal que coordina todas las comprobaciones de colisión.
checkCollisions :: [Robot] -> [Projectile] -> ([Robot], [Projectile])
checkCollisions rs ps = (rs, ps)