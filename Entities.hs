{-# OPTIONS_GHC -Wall #-} -- Advertencias adicionales

module Entities (
    Stage, RobotTurret, Statistics, Projectile
) where

import Geometry (Size, Position, Vector, Scalar, Angle)


data Stage = Stag
  {
    stageSize :: Size
  } deriving(Show , Eq)

data RobotTurret = RobTurr
  {
    turretOrientation :: Angle
  } deriving(Show , Eq)


data Statistics = Stats
  { robots :: Int,
    projectiles :: Int,
    explotions :: Int
  } deriving(Show , Eq)

data Projectile = Proj
  { projectileSize      :: Size
  , projectilePosition  :: Position
  , projectileVelocity  :: Vector
  , projectileDamage    :: Scalar
  } deriving (Show, Eq)