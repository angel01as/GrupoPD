module GameState where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Robot
import Entities
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Geometry

-- Tipo de datos para representar el estado del juego
data GameState = GameState
  { 
    gameRobots :: Map.Map ID Robot,   -- Robots en un map según su ID
    gameProjectiles :: Map.Map ID Projectile, -- Proyectiles
    gameExplosions :: Map.Map ID Explosion,
    gameTime :: Scalar, -- Tiempo actual del juego
    gameWindowSize :: (Int, Int),
    gameStageSize :: Size, -- Tamaño interno del escenario.
    gameBackground :: Maybe Picture,
    gameKeysPressed :: Set.Set Key,
    gameTotalProjectileCount :: Int,
    gameTotalExplosionCount :: Int
  } deriving (Show, Eq)