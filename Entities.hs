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
    projectileDamage    :: Scalar
  } deriving (Show, Eq)

data Explosion = Expl
  {
    explosionID :: ID,
    explosionPosition :: Position,
    explosionRadius :: Scalar,
    explosionMaxRadius :: Scalar,
    explosionDamage :: Scalar,
    explosionTime :: Scalar,
    explosionMaxTime :: Scalar,
    explosionVertices :: [Point]
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

instance GameEntity Explosion where
  position = explosionPosition
  velocity _ = (0, 0) -- Las explosiones no se mueven
  vertices = explosionVertices
  size _ = (0, 0) -- Las explosiones no tienen tamaño fijo
  orientation _ = 0 -- Las explosiones no tienen orientación
  setPosition e pos = e { explosionPosition = pos }
  setVelocity e _ = e -- Las explosiones no cambian de velocidad
  setVertices e verts = e { explosionVertices = verts }
  setSize e _ = e -- Las explosiones no cambian de tamaño
  setOrientation e _ = e -- Las explosiones no cambian de orientación

-- ============================================================================
-- FUNCIONES PARA MANEJAR EXPLOSIONES
-- ============================================================================

-- Crea una nueva explosión
createExplosion :: Position -> Scalar -> Scalar -> Scalar -> Explosion
createExplosion pos maxRadius damage maxTime = Expl
  { explosionPosition = pos
  , explosionRadius = 0
  , explosionMaxRadius = maxRadius
  , explosionDamage = damage
  , explosionTime = 0
  , explosionMaxTime = maxTime
  , explosionVertices = generateExplosionVertices pos 0
  }

-- Genera vértices para una explosión circular
generateExplosionVertices :: Position -> Scalar -> [Point]
generateExplosionVertices (x, y) radius = 
  let numPoints :: Int
      numPoints = 16
      angleStep = 2 * pi / fromIntegral numPoints
      points = [ (x + radius * cos (fromIntegral i * angleStep), 
                 y + radius * sin (fromIntegral i * angleStep)) 
               | i <- [0..numPoints-1] ]
  in points

-- Actualiza una explosión con el tiempo transcurrido
updateExplosion :: Explosion -> Scalar -> Explosion
updateExplosion e deltaTime = 
  let newTime = explosionTime e + deltaTime
      progress = min 1.0 (newTime / explosionMaxTime e)
      newRadius = explosionMaxRadius e * progress
      newVertices = generateExplosionVertices (explosionPosition e) newRadius
  in e { explosionTime = newTime
       , explosionRadius = newRadius
       , explosionVertices = newVertices
       }

-- Verifica si una explosión está activa
isExplosionActive :: Explosion -> Bool
isExplosionActive e = explosionTime e < explosionMaxTime e

-- Verifica si una explosión está en fase de daño (solo los primeros 30%)
isExplosionDamaging :: Explosion -> Bool
isExplosionDamaging e = explosionTime e < explosionMaxTime e * 0.3
