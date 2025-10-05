{-# OPTIONS_GHC -Wall #-} -- Advertencias adicionales

module Entities (
  -- Con tipo data(..) se exporta todo lo que tiene, constructor incluido.
  GameEntity(..), 
  Stage(..), Statistics(..), Projectile(..)
) where

import Geometry (Size, Position, Scalar, Angle, Point, Velocity, add2D, prodByScalar, rotateVertices, translateVertices)

class GameEntity a where
  position :: a -> Position
  velocity :: a -> Velocity
  vertices :: a -> [Point]
  size :: a -> Size
  orientation :: a -> Angle
  setPosition :: a -> Position -> a
  setVelocity :: a -> Velocity -> a
  setVertices :: a -> [Point] -> a
  setSize :: a -> Size -> a
  setOrientation :: a -> Angle -> a
  -- Actualizar una posición en función de la velocidad y el incremento de tiempo. Traslada también los vértices.
  updatePosition :: a -> Scalar -> a
  updatePosition ge t = setVertices (setPosition ge newPos) translatedVerts
    where 
      translation = (prodByScalar t (velocity ge))
      newPos = add2D (position ge) translation
      translatedVerts = translateVertices (vertices ge) translation
  -- Actualiza la orientación y rota los vértices en un ángulo dado.
  updateOrientation :: a -> Angle -> a
  updateOrientation ge angleDiff = setVertices rotatedGE rotatedVerts
    where
      rotatedGE = setOrientation ge (orientation ge + angleDiff)
      rotatedVerts = rotateVertices (vertices ge) angleDiff

data Stage = Stag
  {
    stageSize :: Size
  } deriving(Show , Eq)

data Statistics = Stats
  { robots :: Int,
    projectiles :: Int,
    explotions :: Int
  } deriving(Show , Eq)

data Projectile = Proj
  {
    projectilePosition  :: Position,
    projectileVelocity  :: Velocity,
    projectileVertices  :: [Point],
    projectileSize      :: Size,
    projectileOrientation :: Angle,
    projectileDamage    :: Scalar
  } deriving (Show, Eq)

instance GameEntity Projectile where
  position = projectilePosition
  velocity = projectileVelocity
  vertices = projectileVertices
  size = projectileSize
  orientation = projectileOrientation
  setPosition p pos = p { projectilePosition = pos }
  setVelocity p vel = p { projectileVelocity = vel }
  setVertices p verts = p { projectileVertices = verts }
  setSize p siz = p { projectileSize = siz }
  setOrientation p ori = p { projectileOrientation = ori }
