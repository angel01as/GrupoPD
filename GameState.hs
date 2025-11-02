module GameState where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Robot
import Entities
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Geometry
import UIButton
import Data.Default

-- Tipo de datos para representar el estado del juego
data GameState = GameState
  { 
    gameRobots :: Map.Map ID Robot,   -- Robots en un map según su ID
    gameProjectiles :: Map.Map ID Projectile, -- Proyectiles
    gameExplosions :: Map.Map ID Explosion,
    gameObstacles :: Map.Map ID Obstacle,
    gameBotConfigs :: [(ID, String)], -- Configuración de bots para el menú [(id, tipo)]
    gameTime :: Scalar, -- Tiempo actual del juego
    gameFrame :: Int, -- Frame actual del juego
    gameWindowSize :: (Int, Int),
    gameStageSize :: Size, -- Tamaño interno del escenario.
    gameImages :: Map.Map String Picture,
    gameKeysPressed :: Set.Set Key,
    gameTotalRobotCount :: Int,
    gameTotalProjectileCount :: Int,
    gameTotalExplosionCount :: Int,
    gameSimulationSpeed :: Scalar,
    gameDebugInfo :: Bool,
    gamePaused :: Bool,
    gameButtons :: [UIButton GameState],
    gameSeed :: Double,
    gameIsInMenu :: Bool,
    gameCollisionCooldown :: Scalar
  } deriving (Show, Eq)

-- Estado por defecto.
instance Default GameState where
  def = GameState 
    { 
      gameWindowSize = (500,500),
      gameRobots = Map.empty,
      gameProjectiles = Map.empty,
      gameTime = 0,
      gameFrame = 0,
      gameExplosions = Map.empty,
      gameObstacles = Map.empty,
  gameBotConfigs = [],
      gameStageSize = (100, 50),
      gameImages = Map.empty,
      gameKeysPressed = Set.empty,
      gameTotalRobotCount = 0,
      gameTotalProjectileCount = 0,
      gameTotalExplosionCount = 0,
      gameSimulationSpeed = 1.5,  -- Aumentado de 1 a 1.5 para más velocidad
      gameDebugInfo = False,
      gamePaused = False,
      gameButtons = [],
      gameIsInMenu = True,
      gameSeed = 0,
      gameCollisionCooldown = 0
    }