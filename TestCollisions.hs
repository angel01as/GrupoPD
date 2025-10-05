{-# OPTIONS_GHC -Wall #-}

module Main where

import qualified Data.Map as Map

import Geometry (Point)
import Entities (Projectile(..))
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
  let a = rect2x2 (0,0)
      b = rect2x2 (1,0)     -- solapan
      c = rect2x2 (5,0)     -- no solapan
  print (checkCollision a b)  -- True
  print (checkCollision a c)  -- False

  putStrLn "\n== Pruebas detectRobotProjectileCollisions =="
  let r1 = mkRobot (0,0)
      r2 = mkRobot (5,0)
      p1 = mkProjectile (0.5,0) -- colisiona con r1
      p2 = mkProjectile (10,0)  -- no colisiona
  print (detectRobotProjectileCollisions [p1,p2] [r1,r2])

  putStrLn "\n== Pruebas detectRobotRobotCollisions =="
  let r3 = mkRobot (0,0)
      r4 = mkRobot (0.5,0)  -- colisiona con r3
      r5 = mkRobot (5,0)    -- no colisiona con r3
  print (detectRobotRobotCollisions [r3,r4,r5])

