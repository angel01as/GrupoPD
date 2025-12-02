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
    gameMissiles :: Map.Map ID Missile,
    -- Estadísticas temporales por robot durante un torneo
    gameStats :: Map.Map ID RobotStats,
  -- Tournament control
  gameTournamentActive :: Bool,
  gameTournamentRemaining :: Int,
  gameTournamentSeed :: Double,
  gameTournamentConfigs :: [(ID, String)],
  gameTournamentStatsFile :: Maybe FilePath,  -- Archivo para guardar estadísticas
  gameTournamentCurrentIndex :: Int,  -- Índice del torneo actual
  gameTournamentFileCleared :: Bool, -- Indica si se limpió el archivo de estadísticas al iniciar
  gameTournamentStatsHistory :: [Map.Map ID RobotStats],  -- Historial de estadísticas
  gameTournamentCountdown :: Scalar, -- Cuenta atrás entre torneos (en segundos), 0 si no activa
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
    gameCollisionCooldown :: Scalar,
    gameMenuTimer :: Scalar,  -- Temporizador para inicio automático de torneo
    gameMissileRainTriggered :: Bool,
    gameTotalMissileCount :: Int
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
      gameMissiles = Map.empty,
      gameBotConfigs = [],
      gameStageSize = (100, 50),
      gameImages = Map.empty,
      gameKeysPressed = Set.empty,
      gameTotalRobotCount = 0,
      gameTotalProjectileCount = 0,
      gameTotalExplosionCount = 0,
        gameStats = Map.empty,
      -- Tournament control
      gameTournamentActive = False,
      gameTournamentRemaining = 0,
      gameTournamentSeed = 0,
      gameTournamentConfigs = [],
      gameTournamentStatsFile = Nothing,
      gameTournamentCurrentIndex = 1,
  gameTournamentFileCleared = False,
      gameTournamentStatsHistory = [],
  gameTournamentCountdown = 0,
      gameSimulationSpeed = 1.5,  -- Aumentado de 1 a 1.5 para más velocidad
      gameDebugInfo = False,
      gamePaused = False,
      gameButtons = [],
      gameIsInMenu = True,
      gameSeed = 0,
      gameCollisionCooldown = 0,
      gameMenuTimer = 0,
      gameMissileRainTriggered = False,
      gameTotalMissileCount = 0
    }

  -- Estadísticas por robot durante un torneo
data RobotStats = RobotStats
  { hitsReceived :: Int      -- Proyectiles que recibió el bot
  , hitsLanded :: Int        -- Proyectiles que el bot disparó e impactaron
  , shotsFired :: Int        -- Total de proyectiles disparados por el bot
  , timeAlive :: Scalar      -- Tiempo total con vida
  , kills :: Int             -- Número de robots eliminados por este bot
  } deriving (Show, Eq)

emptyRobotStats :: RobotStats
emptyRobotStats = RobotStats { hitsReceived = 0, hitsLanded = 0, shotsFired = 0, timeAlive = 0, kills = 0 }