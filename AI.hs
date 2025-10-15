{-# OPTIONS_GHC -Wall #-} -- Advertencias adicionales

module AI (
  -- Tipos del DSL
  BotBehavior, 
  BotCondition(..),
  BotCommand(..),
  BotInstruction(..),
  AIExecutionResult(..),
  -- Funciones del DSL
  move, moveBackward, rotate, multiplyVelocity, shoot, wait, ifThen, ifThenElse, sequence,
  -- Condiciones
  hasTarget, isLowEnergy, isUnderAttack, distanceTo, angleTo,
  -- Decisor de comportamientos
  decideBotBehavior,
  -- Ejecutor de comandos
  executeAICommands, updateRobotAI,
  -- Comportamientos de ejemplo
  aggressiveBot, defensiveBot, exampleBot
) where

import Robot (Robot(..), MovementAction(..), Turret(..), MemoryValue(..), isRobotAlive, detectedAgent, updateRobotVelocity, canShoot, shootProjectile, afterShooting, updateTurretCooldown, multiplyMovementAction)
import Entities (Projectile(..), GameEntity(..), Explosion(..), createExplosion, ID)
import Geometry (Position, Angle, Scalar, distanceBetween, angleToTarget, Size)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (minimumBy)
import Prelude hiding (sequence, repeat)
import GameState

-- ============================================================================
-- DSL PARA ACCIONES DEL BOT
-- ============================================================================

-- Comandos básicos que puede ejecutar un bot
data BotCommand
  = MovementCommand MovementAction  -- Comando de movimiento 
  | ShootCommand                    -- Disparar
  | WaitCommand Scalar              -- Esperar un tiempo
  | SetMemoryCommand String MemoryValue  -- Guardar valor en memoria
  | ClearMemoryCommand String       -- Limpiar memoria
  deriving (Show, Eq)

-- Condiciones que puede evaluar un bot
data BotCondition
  = HasTarget                    -- ¿Tiene un enemigo cerca?
  | IsLowEnergy Scalar           -- ¿Energía baja?
  | IsUnderAttack                -- ¿Está siendo atacado?
  | DistanceToTarget Scalar      -- ¿Distancia al enemigo < X?
  | AngleToTarget Angle          -- ¿Ángulo al enemigo < X?
  | MemoryEquals String MemoryValue  -- ¿Memoria tiene valor X?
  | Not BotCondition             -- Negación lógica
  | And BotCondition BotCondition    -- Y lógico
  | Or BotCondition BotCondition     -- O lógico
  deriving (Show, Eq)

-- Instrucciones que puede ejecutar un bot
data BotInstruction
  = Simple BotCommand        -- Comando simple
  | Conditional BotCondition BotInstruction BotInstruction  -- if-then-else
  | Sequence [BotInstruction]   -- Secuencia de instrucciones
  deriving (Show, Eq)

-- Comportamiento del bot que recibe estado del juego y devuelve instrucciones
type BotBehavior = GameState -> Robot -> BotInstruction -- El tipo de dato lo usamos para identificar el tipo de comportamiento del bot.
-- Behavior es una función que recibe el estado del juego y el robot y devuelve una instrucción.

-- ============================================================================
-- FUNCIONES DEL DSL
-- ============================================================================

-- Comandos básicos
move :: Scalar -> BotInstruction
move speed = Simple (MovementCommand (MoveForward speed))

moveBackward :: Scalar -> BotInstruction
moveBackward speed = Simple (MovementCommand (MoveBackward speed))

rotate :: Angle -> BotInstruction
rotate angle = Simple (MovementCommand (Rotate angle))

multiplyVelocity :: Scalar -> BotInstruction
multiplyVelocity factor = Simple (MovementCommand (MultiplyVelocity factor))

shoot :: BotInstruction
shoot = Simple ShootCommand

wait :: Scalar -> BotInstruction
wait time = Simple (WaitCommand time)

setMemory :: String -> MemoryValue -> BotInstruction
setMemory key value = Simple (SetMemoryCommand key value)

clearMemory :: String -> BotInstruction
clearMemory key = Simple (ClearMemoryCommand key)

-- Combinadores de instrucciones
ifThen :: BotCondition -> BotInstruction -> BotInstruction
ifThen cond instruction = Conditional cond instruction (Simple (WaitCommand 0))

ifThenElse :: BotCondition -> BotInstruction -> BotInstruction -> BotInstruction
ifThenElse = Conditional

sequence :: [BotInstruction] -> BotInstruction
sequence = Sequence

-- ============================================================================
-- CONDICIONES DEL DSL
-- ============================================================================

hasTarget :: BotCondition
hasTarget = HasTarget

isLowEnergy :: Scalar -> BotCondition
isLowEnergy = IsLowEnergy

isUnderAttack :: BotCondition
isUnderAttack = IsUnderAttack

distanceTo :: Scalar -> BotCondition
distanceTo = DistanceToTarget

angleTo :: Angle -> BotCondition
angleTo = AngleToTarget

memoryEquals :: String -> MemoryValue -> BotCondition -- ¿Tengo guardado en memoria un valor específico?
memoryEquals = MemoryEquals

-- ============================================================================
-- EVALUADOR DE CONDICIONES
-- ============================================================================

-- Evalúa una condición y devuelve True o False
evalCondition :: BotCondition -> GameState -> Robot -> Bool

-- CONDICIÓN: ¿Tiene el robot un objetivo en rango de su radar?
evalCondition HasTarget gs robot = 
  any (\r -> r /= robot && detectedAgent robot r) (gameRobots gs)

-- CONDICIÓN: ¿La energía del robot está por debajo del umbral?
evalCondition (IsLowEnergy threshold) _ robot = 
  robotEnergy robot < threshold -- True si la energía del robot es menor al umbral.

-- CONDICIÓN: ¿Está el robot siendo atacado?
evalCondition IsUnderAttack gs robot = 
  any (\p -> distanceBetween (position robot) (position p) < 5) (gameProjectiles gs)

-- CONDICIÓN: ¿Está el enemigo más cercano a una distancia menor que maxDist?
evalCondition (DistanceToTarget maxDist) gs robot = 
  case findNearestEnemy robot (gameRobots gs) of
    Nothing -> False
    Just enemy -> distanceBetween (position robot) (position enemy) < maxDist

-- CONDICIÓN: ¿Está el enemigo más cercano dentro del ángulo especificado?
evalCondition (AngleToTarget maxAngle) gs robot = 
  case findNearestEnemy robot (gameRobots gs) of
    Nothing -> False
    Just enemy -> abs (angleToTarget (position robot) (position enemy) - orientation robot) < maxAngle

-- CONDICIÓN: ¿La memoria del robot contiene un valor específico?
evalCondition (MemoryEquals key value) _ robot = -- _ es el estado del juego y el robot.
  case Map.lookup key (robotMemory robot) of -- Lookup es una función que busca un valor en un mapa.
    Nothing -> False
    Just memValue -> value == memValue

-- OPERADORES LÓGICOS
evalCondition (Not cond) gs robot = not (evalCondition cond gs robot)
evalCondition (And cond1 cond2) gs robot = 
  evalCondition cond1 gs robot && evalCondition cond2 gs robot
evalCondition (Or cond1 cond2) gs robot = 
  evalCondition cond1 gs robot || evalCondition cond2 gs robot

-- Encuentra el enemigo más cercano
findNearestEnemy :: Robot -> Map.Map ID Robot -> Maybe Robot
findNearestEnemy robot enemies = 
  let aliveEnemies = filter (\r -> r /= robot && isRobotAlive r) (Map.elems enemies)
  in if null aliveEnemies 
     then Nothing
     else Just (minimumBy (\a b -> compare (distanceBetween (position robot) (position a)) 
                                           (distanceBetween (position robot) (position b))) aliveEnemies)

-- ============================================================================
-- DECISOR DE COMPORTAMIENTOS
-- ============================================================================

-- Decide qué instrucciones debe ejecutar un bot basado en su comportamiento y el estado del juego
decideBotBehavior :: BotBehavior -> GameState -> Robot -> [BotCommand]
decideBotBehavior botBehavior gs robot = 
  let instruction = botBehavior gs robot -- Compor
  in decideInstruction instruction gs robot

-- Decide qué comandos ejecutar basado en una instrucción
decideInstruction :: BotInstruction -> GameState -> Robot -> [BotCommand]
decideInstruction (Simple cmd) _ _ = [cmd] 
decideInstruction (Conditional cond thenInstruction elseInstruction) gs robot =
  if evalCondition cond gs robot -- si se cumple
  then decideInstruction thenInstruction gs robot
  else decideInstruction elseInstruction gs robot
decideInstruction (Sequence instructions) gs robot =  -- Se vuelve a llamar porque es una secuencia de instrucciones.
  concatMap (\instruction -> decideInstruction instruction gs robot) instructions

-- ============================================================================
-- COMPORTAMIENTOS DE EJEMPLO
-- ============================================================================

-- Bot agresivo que busca enemigos y los ataca
aggressiveBot :: BotBehavior
aggressiveBot gs robot = 
  ifThenElse hasTarget
    (sequence [
      -- Si tiene objetivo, apuntar y disparar
      setMemory "mode" (StringValue "attacking"),
      rotate (angleToTarget (position robot) (position (fromMaybe robot (findNearestEnemy robot (gameRobots gs)))) - orientation robot),
      shoot,
      move 1.0
    ])
    (sequence [
      -- Si no tiene objetivo, buscar
      setMemory "mode" (StringValue "searching"),
      rotate (pi/4),  -- Girar 45 grados
      move 0.5,
      wait 0.1
    ])

-- Bot defensivo que evita enemigos cuando tiene poca energía
defensiveBot :: BotBehavior
defensiveBot gs robot = 
  ifThenElse (isLowEnergy 30)
    (sequence [
      -- Si tiene poca energía, huir
      setMemory "mode" (StringValue "fleeing"),
      moveBackward 2.0,  -- Moverse hacia atrás
      rotate (pi),  -- Girar 180 grados
      wait 1.0
    ])
    (ifThenElse hasTarget
      (sequence [
        -- Si tiene objetivo y energía suficiente, atacar
        setMemory "mode" (StringValue "attacking"),
        rotate (angleToTarget (position robot) (position (fromMaybe robot (findNearestEnemy robot (gameRobots gs)))) - orientation robot),
        shoot
      ])
      (sequence [
        -- Patrullar
        setMemory "mode" (StringValue "patrolling"),
        move 1,
        rotate (pi/8),
        wait 0.2
      ])
    )

stupidBot :: BotBehavior
stupidBot gs robot = move 1

-- Bot de ejemplo simple
exampleBot :: BotBehavior
exampleBot = aggressiveBot

-- ============================================================================
-- EJECUTOR DE COMANDOS AI
-- ============================================================================

-- Resultado de ejecutar comandos AI
data AIExecutionResult = AIExecutionResult
  { updatedRobot :: Robot
  , newProjectiles :: [Projectile]
  , newExplosions :: [Explosion]
  } deriving (Show, Eq)

-- Ejecuta una lista de comandos AI sobre un robot
executeAICommands :: [BotCommand] -> Robot -> Scalar -> AIExecutionResult
executeAICommands commands robot deltaTime = 
  let (updatedRobot', spawnedProjectiles', spawnedExplosions') = 
        foldl (executeCommand deltaTime) (robot, [], []) commands
  in AIExecutionResult updatedRobot' spawnedProjectiles' spawnedExplosions'

-- Ejecuta un comando individual
executeCommand :: Scalar -> (Robot, [Projectile], [Explosion]) -> BotCommand -> (Robot, [Projectile], [Explosion])
executeCommand deltaTime (robot, projectiles, explosions) (MovementCommand action) = 
  (updateRobotVelocity robot (multiplyMovementAction deltaTime action), projectiles, explosions)

executeCommand _ (robot, projectiles, explosions) ShootCommand = 
  case shootProjectile robot of
    Just projectile -> (afterShooting robot, projectile : projectiles, explosions)
    Nothing -> (robot, projectiles, explosions)

executeCommand deltaTime (robot, projectiles, explosions) (WaitCommand time) = 
  (robot, projectiles, explosions) -- El wait se maneja a nivel de instrucción

executeCommand _ (robot, projectiles, explosions) (SetMemoryCommand key value) = 
  (robot { robotMemory = Map.insert key value (robotMemory robot) }, projectiles, explosions)

executeCommand _ (robot, projectiles, explosions) (ClearMemoryCommand key) = 
  (robot { robotMemory = Map.delete key (robotMemory robot) }, projectiles, explosions)

-- Actualiza un robot con su comportamiento AI
updateRobotAI :: Robot -> GameState -> Scalar -> AIExecutionResult
updateRobotAI robot gs deltaTime = 
  let -- Actualizar cooldown de la torreta
      robotWithUpdatedCooldown = updateTurretCooldown robot deltaTime
      
      -- Obtener comportamiento por nombre
      behavior = getBehaviorByName (robotBehavior robot)
      
      -- Decidir comportamiento
      commands = decideBotBehavior behavior gs robotWithUpdatedCooldown
      
      -- Ejecutar comandos
      result = executeAICommands commands robotWithUpdatedCooldown deltaTime
      
      -- Actualizar tiempo de última actualización
      finalRobot = (updatedRobot result) { robotLastUpdateTime = gameTime gs }
  in result { updatedRobot = finalRobot }

-- Obtiene un comportamiento por su nombre
getBehaviorByName :: String -> BotBehavior
getBehaviorByName "aggressive" = aggressiveBot
getBehaviorByName "defensive" = defensiveBot
getBehaviorByName "stupid" = stupidBot
getBehaviorByName _ = aggressiveBot -- Comportamiento por defecto
