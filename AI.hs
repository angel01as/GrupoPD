{-# OPTIONS_GHC -Wall #-} -- Advertencias adicionales
{-# LANGUAGE DeriveFunctor #-}

module AI (
  -- Tipos de acciones del DSL
  BotAction,
  BotCondition(..),
  BotCommand(..),
  BotBehavior(..),
  GameState(..),
  -- Funciones del DSL
  move, moveBackward, rotate, multiplyVelocity, shoot, wait, ifThen, ifThenElse, repeat, sequence, parallel,
  -- Condiciones
  hasTarget, isLowEnergy, isUnderAttack, distanceTo, angleTo,
  -- Ejemplo de bot
  exampleBot, aggressiveBot, defensiveBot,
  -- Evaluador del DSL
  executeBotAction
) where

import Robot (Robot(..), MovementAction(..), Turret(..), MemoryValue(..), isRobotAlive, detectedAgent)
import Entities (Projectile(..), GameEntity(..))
import Geometry (Position, Angle, Scalar, distanceBetween, angleToTarget)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (minimumBy)
import Prelude hiding (sequence, repeat)

-- ============================================================================
-- DSL PARA ACCIONES DEL BOT
-- ============================================================================

-- Tipo de datos para representar el estado del juego
data GameState = GameState
  { gameRobots :: [Robot]   -- Lista de todos los robots
  , gameProjectiles :: [Projectile] -- Lista de todos los proyectiles
  , gameTime :: Scalar -- Tiempo actual del juego
  } deriving (Show, Eq)

-- Comandos básicos que puede ejecutar un bot
data BotCommand
  = MovementCmd MovementAction  -- Comando de movimiento 
  | ShootCmd                    -- Disparar
  | WaitCmd Scalar              -- Esperar un tiempo
  | SetMemoryCmd String MemoryValue  -- Guardar valor en memoria
  | ClearMemoryCmd String       -- Limpiar memoria
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

-- Comportamientos complejos del bot
data BotBehavior
  = Simple BotCommand            -- Comando simple
  | Conditional BotCondition BotBehavior BotBehavior  -- if-then-else
  | Sequence [BotBehavior]       -- Secuencia de acciones
  | Parallel [BotBehavior]       -- Acciones paralelas
  | Repeat Int BotBehavior       -- Repetir N veces
  | Loop BotBehavior             -- Bucle infinito
  | Try BotBehavior BotBehavior  -- Intentar A, si falla hacer B
  deriving (Show, Eq)

-- Acción principal del bot que recibe estado del juego y devuelve comportamiento
type BotAction = GameState -> Robot -> BotBehavior

-- ============================================================================
-- FUNCIONES DEL DSL
-- ============================================================================

-- Comandos básicos
move :: Scalar -> BotBehavior
move speed = Simple (MovementCmd (MoveForward speed))

moveBackward :: Scalar -> BotBehavior
moveBackward speed = Simple (MovementCmd (MoveBackward speed))

rotate :: Angle -> BotBehavior
rotate angle = Simple (MovementCmd (Rotate angle))

multiplyVelocity :: Scalar -> BotBehavior
multiplyVelocity factor = Simple (MovementCmd (MultiplyVelocity factor))

shoot :: BotBehavior
shoot = Simple ShootCmd

wait :: Scalar -> BotBehavior
wait time = Simple (WaitCmd time)

setMemory :: String -> MemoryValue -> BotBehavior
setMemory key value = Simple (SetMemoryCmd key value)

clearMemory :: String -> BotBehavior
clearMemory key = Simple (ClearMemoryCmd key)

-- Combinadores de comportamiento
ifThen :: BotCondition -> BotBehavior -> BotBehavior
ifThen cond behavior = Conditional cond behavior (Simple (WaitCmd 0))

ifThenElse :: BotCondition -> BotBehavior -> BotBehavior -> BotBehavior
ifThenElse = Conditional

sequence :: [BotBehavior] -> BotBehavior
sequence = Sequence

parallel :: [BotBehavior] -> BotBehavior
parallel = Parallel

repeat :: Int -> BotBehavior -> BotBehavior
repeat = Repeat

loop :: BotBehavior -> BotBehavior
loop = Loop

try :: BotBehavior -> BotBehavior -> BotBehavior
try = Try

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

memoryEquals :: String -> MemoryValue -> BotCondition
memoryEquals = MemoryEquals

-- ============================================================================
-- EVALUADOR DEL DSL
-- ============================================================================

-- ============================================================================
-- EVALUADOR DE CONDICIONES
-- ============================================================================

-- Evalúa una condición y devuelve True o False
-- Esta función es el corazón del sistema de toma de decisiones del bot
evalCondition :: BotCondition -> GameState -> Robot -> Bool

-- CONDICIÓN: ¿Tiene el robot un objetivo en rango de su radar?
-- Usa la función detectAgent existente para verificar si hay enemigos cerca
evalCondition HasTarget gs robot = 
  any (\r -> r /= robot && detectedAgent robot r) (gameRobots gs)

-- CONDICIÓN: ¿La energía del robot está por debajo del umbral?
-- Compara la energía actual con el umbral especificado
evalCondition (IsLowEnergy threshold) _ robot = robotEnergy robot < threshold

-- CONDICIÓN: ¿Está el robot siendo atacado?
-- Verifica si hay proyectiles cerca (distancia < 5 unidades)
evalCondition IsUnderAttack gs robot = 
  any (\p -> distanceBetween (position robot) (position p) < 5) (gameProjectiles gs)

-- CONDICIÓN: ¿Está el enemigo más cercano a una distancia menor que maxDist?
-- Busca el enemigo más cercano y verifica la distancia
evalCondition (DistanceToTarget maxDist) gs robot = 
  case findNearestEnemy robot (gameRobots gs) of
    Nothing -> False  -- No hay enemigos, condición falsa
    Just enemy -> distanceBetween (position robot) (position enemy) < maxDist

-- CONDICIÓN: ¿Está el enemigo más cercano dentro del ángulo especificado?
-- Calcula el ángulo hacia el enemigo y lo compara con la orientación del robot
evalCondition (AngleToTarget maxAngle) gs robot = 
  case findNearestEnemy robot (gameRobots gs) of
    Nothing -> False  -- No hay enemigos, condición falsa
    Just enemy -> abs (angleToTarget (position robot) (position enemy) - robotOrientation robot) < maxAngle

-- CONDICIÓN: ¿La memoria del robot contiene un valor específico?
-- Busca en la memoria del robot y compara con el valor dado
evalCondition (MemoryEquals key value) _ robot = 
  case Map.lookup key (robotMemory robot) of
    Nothing -> False  -- La clave no existe en memoria
    Just memValue -> value == memValue  -- Compara los valores

-- OPERADORES LÓGICOS: Permiten combinar condiciones de forma compleja

-- NEGACIÓN: Invierte el resultado de una condición
evalCondition (Not cond) gs robot = not (evalCondition cond gs robot)

-- CONJUNCIÓN: Ambas condiciones deben ser verdaderas
evalCondition (And cond1 cond2) gs robot = 
  evalCondition cond1 gs robot && evalCondition cond2 gs robot

-- DISYUNCIÓN: Al menos una condición debe ser verdadera
evalCondition (Or cond1 cond2) gs robot = 
  evalCondition cond1 gs robot || evalCondition cond2 gs robot

-- Encuentra el enemigo más cercano
findNearestEnemy :: Robot -> [Robot] -> Maybe Robot
findNearestEnemy robot enemies = 
  let aliveEnemies = filter (\r -> r /= robot && isRobotAlive r) enemies
  in if null aliveEnemies 
     then Nothing
     else Just (minimumBy (\a b -> compare (distanceBetween (position robot) (position a)) 
                                           (distanceBetween (position robot) (position b))) aliveEnemies)

-- Ejecuta una acción del bot y devuelve el robot actualizado
executeBotAction :: BotAction -> GameState -> Robot -> (Robot, [BotCommand])
executeBotAction botAction gs robot = 
  let behavior = botAction gs robot
      (updatedRobot, commands) = executeBehavior behavior gs robot
  in (updatedRobot, commands)

-- ============================================================================
-- EJECUTOR DE COMPORTAMIENTOS
-- ============================================================================

-- Función principal que ejecuta cualquier comportamiento
executeBehavior :: BotBehavior -> GameState -> Robot -> (Robot, [BotCommand])
executeBehavior (Simple cmd) = executeSimple cmd
executeBehavior (Conditional cond thenB elseB) = executeConditional cond thenB elseB
executeBehavior (Sequence behaviors) = executeSequence behaviors
executeBehavior (Parallel behaviors) = executeParallel behaviors
executeBehavior (Repeat n behavior) = executeRepeat n behavior
executeBehavior (Loop behavior) = executeLoop behavior
executeBehavior (Try behavior fallback) = executeTry behavior fallback

-- ============================================================================
-- FUNCIONES AUXILIARES DE EJECUCIÓN
-- ============================================================================

-- Ejecuta un comando simple
executeSimple :: BotCommand -> GameState -> Robot -> (Robot, [BotCommand])
executeSimple cmd _ robot = (robot, [cmd])

-- Ejecuta un condicional (if-then-else)
executeConditional :: BotCondition -> BotBehavior -> BotBehavior -> GameState -> Robot -> (Robot, [BotCommand])
executeConditional cond thenBehavior elseBehavior gs robot =
  if evalCondition cond gs robot
  then executeBehavior thenBehavior gs robot
  else executeBehavior elseBehavior gs robot

-- Ejecuta una secuencia de comportamientos (uno tras otro)
executeSequence :: [BotBehavior] -> GameState -> Robot -> (Robot, [BotCommand])
executeSequence behaviors gs robot = 
  foldl (\(r, cmds) behavior -> 
    let (newR, newCmds) = executeBehavior behavior gs r
    in (newR, cmds ++ newCmds)) (robot, []) behaviors

-- Ejecuta comportamientos en paralelo (todos a la vez)
executeParallel :: [BotBehavior] -> GameState -> Robot -> (Robot, [BotCommand])
executeParallel behaviors gs robot = 
  let results = map (\behavior -> executeBehavior behavior gs robot) behaviors
      allCommands = concatMap snd results
  in (robot, allCommands)  -- En paralelo, no actualizamos el robot múltiples veces

-- Ejecuta un comportamiento N veces
executeRepeat :: Int -> BotBehavior -> GameState -> Robot -> (Robot, [BotCommand])
executeRepeat n behavior gs robot = 
  if n <= 0 
  then (robot, [])
  else let (newRobot, cmds) = executeBehavior behavior gs robot
           (finalRobot, moreCmds) = executeRepeat (n-1) behavior gs newRobot
       in (finalRobot, cmds ++ moreCmds)

-- Ejecuta un bucle infinito (con límite de seguridad)
executeLoop :: BotBehavior -> GameState -> Robot -> (Robot, [BotCommand])
executeLoop behavior gs robot = 
  executeRepeat 100 behavior gs robot  -- Límite de seguridad para evitar bucles infinitos

-- Intenta ejecutar un comportamiento, si falla ejecuta el fallback
executeTry :: BotBehavior -> BotBehavior -> GameState -> Robot -> (Robot, [BotCommand])
executeTry behavior fallback gs robot = 
  let (newRobot, cmds) = executeBehavior behavior gs robot
  in if null cmds  -- Si no se ejecutó nada, usar fallback
     then executeBehavior fallback gs robot
     else (newRobot, cmds)

-- ============================================================================
-- EJEMPLO DE BOT
-- ============================================================================

-- Bot agresivo que busca enemigos y los ataca
aggressiveBot :: BotAction
aggressiveBot gs robot = 
  ifThenElse hasTarget
    (sequence [
      -- Si tiene objetivo, apuntar y disparar
      setMemory "lastTarget" (PositionValue (position (fromMaybe robot (findNearestEnemy robot (gameRobots gs))))),
      rotate (angleToTarget (position robot) (position (fromMaybe robot (findNearestEnemy robot (gameRobots gs)))) - robotOrientation robot),
      shoot,
      move 1.0
    ])
    (sequence [
      -- Si no tiene objetivo, buscar
      setMemory "searchMode" (BoolValue True),
      rotate (pi/4),  -- Girar 45 grados
      move 0.5,
      wait 0.1
    ])

-- Bot defensivo que evita enemigos y se cura
defensiveBot :: BotAction
defensiveBot gs robot = 
  ifThenElse (isLowEnergy 30)
    (sequence [
      -- Si tiene poca energía, huir y buscar energía
      setMemory "mode" (StringValue "fleeing"),
      moveBackward 2.0,  -- Moverse hacia atrás
      rotate (pi),  -- Girar 180 grados
      wait 1.0
    ])
    (ifThenElse isUnderAttack
      (sequence [
        -- Si está bajo ataque, evadir
        setMemory "mode" (StringValue "evading"),
        rotate (pi/2),
        move 1.5,
        wait 0.5
      ])
      (ifThenElse hasTarget
        (sequence [
          -- Si tiene objetivo y está seguro, atacar
          setMemory "mode" (StringValue "attacking"),
          rotate (angleToTarget (position robot) (position (fromMaybe robot (findNearestEnemy robot (gameRobots gs)))) - robotOrientation robot),
          shoot
        ])
        (sequence [
          -- Patrullar
          setMemory "mode" (StringValue "patrolling"),
          move 0.3,
          rotate (pi/8),
          wait 0.2
        ])
      )
    )

-- Bot inteligente que usa memoria para tomar decisiones
intelligentBot :: BotAction
intelligentBot gs robot = 
  let lastMode = getMemoryString "mode" robot
  in case lastMode of
    "hunting" -> huntingBehavior gs robot
    "retreating" -> retreatingBehavior gs robot
    "patrolling" -> patrollingBehavior gs robot
    _ -> initialBehavior gs robot

-- Comportamiento de caza
huntingBehavior :: GameState -> Robot -> BotBehavior
huntingBehavior gs robot = 
  ifThenElse hasTarget
    (sequence [
      setMemory "lastSeenTarget" (PositionValue (position (fromMaybe robot (findNearestEnemy robot (gameRobots gs))))),
      rotate (angleToTarget (position robot) (position (fromMaybe robot (findNearestEnemy robot (gameRobots gs)))) - robotOrientation robot),
      shoot,
      move 1.0
    ])
    (ifThenElse (memoryEquals "lastSeenTarget" (PositionValue (0,0)))
      (sequence [
        setMemory "mode" (StringValue "patrolling"),
        move 0.5,
        rotate (pi/4)
      ])
      (sequence [
        -- Buscar en la última posición conocida
        setMemory "mode" (StringValue "patrolling"),
        move 0.3,
        rotate (pi/8)
      ])
    )

-- Comportamiento de retirada
retreatingBehavior :: GameState -> Robot -> BotBehavior
retreatingBehavior gs robot = 
  ifThenElse (isLowEnergy 20)
    (sequence [
      moveBackward 1.5,
      rotate (pi),
      wait 2.0,
      setMemory "mode" (StringValue "patrolling")
    ])
    (sequence [
      setMemory "mode" (StringValue "hunting"),
      move 0.5
    ])

-- Comportamiento de patrullaje
patrollingBehavior :: GameState -> Robot -> BotBehavior
patrollingBehavior gs robot = 
  ifThenElse hasTarget
    (sequence [
      setMemory "mode" (StringValue "hunting"),
      setMemory "targetCount" (IntValue (getMemoryInt "targetCount" robot + 1))
    ])
    (ifThenElse (isLowEnergy 40)
      (sequence [
        setMemory "mode" (StringValue "retreating")
      ])
      (sequence [
        move 0.3,
        rotate (pi/6),
        wait 0.1
      ])
    )

-- Comportamiento inicial
initialBehavior :: GameState -> Robot -> BotBehavior
initialBehavior gs robot = 
  sequence [
    setMemory "mode" (StringValue "patrolling"),
    setMemory "targetCount" (IntValue 0),
    move 0.5
  ]

-- Funciones auxiliares para acceder a la memoria
getMemoryString :: String -> Robot -> String
getMemoryString key robot = 
  case getMemoryWithDefault (StringValue "") key robot of
    StringValue s -> s
    _ -> ""

getMemoryInt :: String -> Robot -> Int
getMemoryInt key robot = 
  case getMemoryWithDefault (IntValue 0) key robot of
    IntValue i -> i
    _ -> 0

-- Función genérica para obtener valores de memoria con valor por defecto
getMemoryWithDefault :: MemoryValue -> String -> Robot -> MemoryValue
getMemoryWithDefault defaultValue key robot = 
  Map.findWithDefault defaultValue key (robotMemory robot)

-- Bot de ejemplo que combina diferentes estrategias
exampleBot :: BotAction
exampleBot = intelligentBot
