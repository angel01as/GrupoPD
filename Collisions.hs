{-# OPTIONS_GHC -Wall #-}

module Collisions
  ( checkCollision
  , detectRobotProjectileCollisions
  , detectRobotRobotCollisions
  , detectRobotObstacleCollisions
  , detectProjectileObstacleCollisions
  , checkCollisions
  , willCollideNextFrame
  , RobotProjectileCollisionEvent
  , RobotRobotCollisionEvent
  ) where

import Geometry
  (Scalar2D, Point, Vector
  , dot, sub, perp )

import Entities (Projectile(..), GameEntity (position, vertices, velocity), Obstacle(..))
import Robot (Robot(..))
import Data.Maybe
import Geometry (prodByScalar, translateVertices)

-- Eventos de colisión con type
type RobotProjectileCollisionEvent = (Projectile, Robot)
type RobotRobotCollisionEvent      = (Robot, Robot)



-- Alg. SAT
-- Tomar vertices de un polígono y crear vectores de sus lados
-- Tomar las perpendiculares de esos vectores
-- Para cada perpendicular calcular la proyección de los vectores de los puntos en estas
-- Comprobar el cruce de los rangos del minimo al maximo de cada poligono, si en uno de estos NO hay cruce, no colisionan

-- checkCollision: Comprueba si dos rectángulos han colisionado utilizando el algoritmo apropiado.
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

-- Comprueba si dos rangos intersecan.
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
detectRobotRobotCollisions (r:rs) =
  colisionesConR r rs ++ detectRobotRobotCollisions rs
  where
    -- Compara el robot r con cada robot del resto de la lista
    colisionesConR :: Robot -> [Robot] -> [RobotRobotCollisionEvent]
    colisionesConR a = mapMaybe (colisionRobot a)

    colisionRobot :: Robot -> Robot -> Maybe (Robot, Robot)
    colisionRobot ra rb = if checkCollision (vertices ra) (vertices rb)
                          then Just (ra,rb)
                          else Nothing
--     colisionesConR _ [] = []
--     colisionesConR a (b:bs)
--       | checkCollision (robotVertices a) (robotVertices b) = (a, b) : colisionesConR a bs
--       | otherwise                                          = colisionesConR a bs

-- checkCollisions: Función principal que coordina todas las comprobaciones de colisión.
checkCollisions :: [Robot] -> [Projectile] -> ([RobotProjectileCollisionEvent], [RobotRobotCollisionEvent])
checkCollisions rs ps = (detectRobotProjectileCollisions ps rs, detectRobotRobotCollisions rs)

-- Detección de colisiones Robot–Obstáculo
detectRobotObstacleCollisions :: [Robot] -> [Obstacle] -> [(Robot, Obstacle, (Float,Float))]
detectRobotObstacleCollisions rs obs = catMaybes [ collide r o | r <- rs, o <- obs ]
  where
    collide r o =
      if checkCollision (vertices r) (vertices o)
        then let (rx, ry) = position r
                 (ox, oy) = obstaclePosition o
                 vx = rx - ox; vy = ry - oy
                 m = sqrt (vx*vx + vy*vy)
                 dir = if m < 1e-6 then (0,0) else (vx/m, vy/m)
                 push = prodByScalar 0.8 dir
             in Just (r, o, push)
        else Nothing

-- Detección de colisiones Proyectil–Obstáculo
detectProjectileObstacleCollisions :: [Projectile] -> [Obstacle] -> [(Projectile, Obstacle)]
detectProjectileObstacleCollisions ps obs = catMaybes [ if checkCollision (vertices p) (vertices o) then Just (p,o) else Nothing | p <- ps, o <- obs ]

-- Predicción simple: ¿colisionará un robot con un obstáculo en el próximo frame si avanza con su velocidad actual?
-- Usa los vértices trasladados por v*dt para hacer un SAT contra el obstáculo.
willCollideNextFrame :: Robot -> Obstacle -> Float -> Bool
willCollideNextFrame r o dt =
  let nextVerts = translateVertices (vertices r) (prodByScalar dt (velocity r))
  in checkCollision nextVerts (vertices o)