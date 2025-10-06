{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Main where

import qualified Data.Map as Map

import Geometry (Point, translateVertices, rotateVertices)
import Entities (Projectile(..), GameEntity(..))
import Robot (Robot(..), Turret(..))
import Collisions (checkCollision, detectRobotProjectileCollisions, detectRobotRobotCollisions)

-- Construye un rectángulo axis-aligned de 2x2 centrado en (cx, cy)
rect2x2 :: (Double, Double) -> [Point]
rect2x2 (cx, cy) =
  [ (cx - 1, cy - 1)
  , (cx + 1, cy - 1)
  , (cx + 1, cy + 1)
  , (cx - 1, cy + 1)
  ]

mkRobot :: (Double, Double) -> Robot
mkRobot (x, y) = Rob
  { robotPosition   = (x, y)
  , robotVelocity   = (0, 0)
  , robotSize       = (2, 2)
  , robotVertices   = rect2x2 (x, y)
  , robotEnergy     = 100
  , robotRadarRange = 50
  , robotOrientation= 0
  , robotTurret     = Turr 0
  , robotMemory     = Map.empty
  }

mkProjectile :: (Double, Double) -> Projectile
mkProjectile (x, y) = Proj
  { projectilePosition    = (x, y)
  , projectileVelocity    = (0, 0)
  , projectileVertices    = rect2x2 (x, y)
  , projectileSize        = (2, 2)
  , projectileOrientation = 0
  , projectileDamage      = 25
  }

main :: IO ()
main = do
  putStrLn "== Pruebas checkCollision =="
  let c1 = (0,0)
      c2 = (1,0)
      c3 = (5,0)
      c4 = (2,0)
  let a = rect2x2 c1
      b = rect2x2 c2     -- solapan
      c = rect2x2 c3     -- no solapan
      d = rect2x2 c4
  putStrLn "Entre centros:"
  print [c1,c2,c3,c4]
  putStrLn "Resultado:"
  print (checkCollision a b)  -- True
  print (checkCollision a c)  -- False
  print $ checkCollision a d -- True, por poco
  print $ checkCollision a (translateVertices d (0.1,0)) --False, ya no colisionan.
  print $ checkCollision a (rotateVertices (translateVertices d (0.1,0)) (pi/4)) --True, con el giro vuelven a chocar.

  putStrLn "\n== Pruebas detectRobotProjectileCollisions =="
  let r1 = mkRobot (0,0)
      r2 = mkRobot (5,0)
      p1 = mkProjectile (0.5,0) -- colisiona con r1
      p2 = mkProjectile (10,0)  -- no colisiona
  putStrLn "Entre:"
  prettyPrint [p1,p2]
  prettyPrint [r1,r2]
  putStrLn "Resultado:"
  prettyPrint (detectRobotProjectileCollisions [p1,p2] [r1,r2])

  putStrLn "\n== Pruebas detectRobotRobotCollisions =="
  let r3 = mkRobot (0,0)
      r4 = mkRobot (0.5,0)  -- colisiona con r3
      r5 = mkRobot (5,0)    -- no colisiona con r3
  putStrLn "Entre:"
  prettyPrint [r3,r4,r5]
  putStrLn "Resultado:"
  prettyPrint (detectRobotRobotCollisions [r3,r4,r5])


class PrettyShow a where
  showReadable :: a -> String

instance PrettyShow Robot where
  showReadable r = "Robot { Position = " ++ (show $ position r) ++ " }"

instance PrettyShow Projectile where
  showReadable p = "Projectile { Position = " ++ (show $ position p) ++ " }"

-- Instancia para listas
instance PrettyShow a => PrettyShow [a] where
  showReadable xs = "[" ++ go xs ++ "]"
    where
      go []     = ""
      go [y]    = showReadable y
      go (y:ys) = showReadable y ++ ", " ++ go ys

-- Instancia para tuplas de dos elementos
instance (PrettyShow a, PrettyShow b) => PrettyShow (a,b) where
  showReadable (a,b) = "(" ++ showReadable a ++ ", " ++ showReadable b ++ ")"


-- Instancia por defecto para todo lo que tenga Show
instance {-# OVERLAPPABLE #-} Show a => PrettyShow a where
  showReadable = show

prettyPrint :: PrettyShow a => a -> IO ()
prettyPrint = putStrLn . showReadable