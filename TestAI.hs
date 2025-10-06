{-# OPTIONS_GHC -Wall #-}

module Main where

import qualified Data.Map as Map
import Geometry (Point)
import Entities (Projectile(..))
import Robot (Robot(..), Turret(..), MovementAction(..))
import AI (GameState(..), exampleBot, aggressiveBot, defensiveBot, decideBotBehavior, BotCommand(..))

-- Construye un rectángulo axis-aligned de 2x2 centrado en (cx, cy)
rect2x2 :: (Double, Double) -> [Point]
rect2x2 (cx, cy) =
  [ (cx - 1, cy - 1)
  , (cx + 1, cy - 1)
  , (cx + 1, cy + 1)
  , (cx - 1, cy + 1)
  ]

-- Crear un robot de prueba
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

-- Crear un proyectil de prueba
mkProjectile :: (Double, Double) -> Projectile
mkProjectile (x, y) = Proj
  { projectilePosition    = (x, y)
  , projectileVelocity    = (0, 0)
  , projectileVertices    = rect2x2 (x, y)
  , projectileSize        = (2, 2)
  , projectileOrientation = 0
  , projectileDamage      = 25
  }

-- Crear estado de juego de prueba
mkGameState :: [Robot] -> [Projectile] -> GameState
mkGameState robots projectiles = GameState
  { gameRobots = robots
  , gameProjectiles = projectiles
  , gameTime = 0.0
  }

-- Función para mostrar comandos de forma legible
showCommand :: BotCommand -> String
showCommand (MovementCommand (MoveForward speed)) = "MoverAdelante(" ++ show speed ++ ")"
showCommand (MovementCommand (MoveBackward speed)) = "MoverAtras(" ++ show speed ++ ")"
showCommand (MovementCommand (Rotate angle)) = "Rotar(" ++ show angle ++ ")"
showCommand (MovementCommand (MultiplyVelocity factor)) = "MultiplicarVelocidad(" ++ show factor ++ ")"
showCommand ShootCommand = "Disparar"
showCommand (WaitCommand time) = "Esperar(" ++ show time ++ ")"
showCommand (SetMemoryCommand key value) = "GuardarMemoria(" ++ key ++ ", " ++ show value ++ ")"
showCommand (ClearMemoryCommand key) = "LimpiarMemoria(" ++ key ++ ")"

-- Función para mostrar lista de comandos
showCommands :: [BotCommand] -> String
showCommands [] = "[]"
showCommands cmds = "[" ++ go cmds ++ "]"
  where
    go [] = ""
    go [c] = showCommand c
    go (c:cs) = showCommand c ++ ", " ++ go cs

main :: IO ()
main = do
  putStrLn "=== DEMOSTRACIÓN DEL DSL PARA BOTS ==="
  
  -- Crear robots de prueba
  let robot1 = mkRobot (0, 0)
      robot2 = mkRobot (10, 0)  -- Enemigo lejano
      robot3 = mkRobot (3, 0)   -- Enemigo cercano
      projectiles = [mkProjectile (1, 0)]  -- Proyectil cerca
  
  putStrLn "\n1. ROBOT AGRESIVO:"
  putStrLn "   Robot en (0,0) con enemigo en (3,0)"
  let gs1 = mkGameState [robot1, robot3] projectiles
      commands1 = decideBotBehavior aggressiveBot gs1 robot1
  putStrLn $ "   Comandos generados: " ++ showCommands commands1
  putStrLn $ "   Energía del robot: " ++ show (robotEnergy robot1)
  
  putStrLn "\n2. ROBOT DEFENSIVO:"
  putStrLn "   Robot con poca energía (20) y bajo ataque"
  let lowEnergyRobot = robot1 { robotEnergy = 20 }
      gs2 = mkGameState [lowEnergyRobot, robot3] projectiles
      commands2 = decideBotBehavior defensiveBot gs2 lowEnergyRobot
  putStrLn $ "   Comandos generados: " ++ showCommands commands2
  putStrLn $ "   Energía del robot: " ++ show (robotEnergy lowEnergyRobot)
  
  putStrLn "\n3. ROBOT INTELIGENTE (EJEMPLO):"
  putStrLn "   Robot que usa memoria para tomar decisiones"
  let gs3 = mkGameState [robot1, robot2] []
      commands3 = decideBotBehavior exampleBot gs3 robot1
  putStrLn $ "   Comandos generados: " ++ showCommands commands3
  putStrLn $ "   Memoria del robot: " ++ show (robotMemory robot1)
  
  putStrLn "\n4. PRUEBA DE CONDICIONES:"
  let gs4 = mkGameState [robot1, robot3] projectiles
      commands4 = decideBotBehavior exampleBot gs4 robot1
  putStrLn $ "   Con enemigo cercano y proyectil: " ++ showCommands commands4
  
  putStrLn "\n=== FIN DE LA DEMOSTRACIÓN ==="
  putStrLn "\nEl DSL permite crear bots con comportamientos complejos:"
  putStrLn "- Comandos básicos: mover, rotar, disparar, esperar"
  putStrLn "- Condiciones: hasTarget, isLowEnergy, isUnderAttack"
  putStrLn "- Combinadores: ifThen, sequence, parallel, repeat"
  putStrLn "- Memoria: para almacenar información entre decisiones"
  putStrLn "- Comportamientos: agresivo, defensivo, inteligente"
