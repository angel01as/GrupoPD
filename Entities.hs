{-# OPTIONS_GHC -Wall #-} -- Advertencias adicionales

module Entities (
  -- Con tipo data(..) se exporta todo lo que tiene, constructor incluido.
  GameEntity(..), Projectile(..), Explosion(..), ID,
  createExplosion, updateExplosion, isExplosionActive, isExplosionDamaging
) where

import Geometry (Size, Position, Scalar, Angle, Point, Velocity, add2D, prodByScalar, rotateVertices, translateVertices)

type ID = Int

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

data Projectile = Proj
  {
    projectileID :: ID,
    projectilePosition  :: Position,
    projectileVelocity  :: Velocity,
    projectileVertices  :: [Point],
    projectileSize      :: Size,
    projectileOrientation :: Angle,
    projectileDamage    :: Scalar,
    projectileOwnerID :: ID -- ID del Robot que lo disparó.
  } deriving (Show, Eq)

data Explosion = Expl
  {
    explosionID :: ID,
    explosionPosition :: Position,
    explosionRadius :: Scalar,
    explosionMaxRadius :: Scalar,
    explosionDamage :: Scalar,
    explosionTime :: Scalar,
    explosionMaxTime :: Scalar
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

-- ============================================================================
-- FUNCIONES PARA MANEJAR EXPLOSIONES
-- ============================================================================

-- Crea una nueva explosión
createExplosion :: Position -> Scalar -> Scalar -> Scalar -> ID -> Explosion
createExplosion pos maxRadius damage maxTime newID = Expl
  { 
    explosionPosition = pos,
    explosionRadius = 0,
    explosionMaxRadius = maxRadius,
    explosionDamage = damage,
    explosionTime = 0,
    explosionMaxTime = maxTime,
    explosionID = newID
  }

-- Actualiza una explosión con el tiempo transcurrido
updateExplosion :: Explosion -> Scalar -> Explosion
updateExplosion e deltaTime = 
  let newTime = explosionTime e + deltaTime
      progress = min 1.0 (newTime / explosionMaxTime e)
      newRadius = explosionMaxRadius e * progress
  in e { explosionTime = newTime
       , explosionRadius = newRadius
       }

-- Verifica si una explosión está activa
isExplosionActive :: Explosion -> Bool
isExplosionActive e = explosionTime e < explosionMaxTime e

-- Verifica si una explosión está en fase de daño (solo los primeros 30%)
isExplosionDamaging :: Explosion -> Bool
isExplosionDamaging e = explosionTime e < explosionMaxTime e * 0.3
