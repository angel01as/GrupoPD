import Game
import Robot (createBasicRobot)
import GameState
import RandomUtils (generatePositionFromSeed)

import qualified Data.Map as Map (fromList)
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (getPOSIXTime)

-- Punto de entrada al juego.

main :: IO()
main = do
    -- Estado inicial del torneo: robots en posiciones aleatorias
    let window = (1000, 700)  -- Tamaño de la ventana en píxeles (ancho, alto)
    let stageSize = (100, 70)  -- Tamaño del escenario de juego en unidades internas (ancho, alto)
    
    -- Obtener semilla inicial del tiempo del sistema
    time <- getPOSIXTime
    let seedBase = realToFrac time  -- Convertir el tiempo a Float para usarlo como semilla
    
    -- Calcular los límites (bounds) del área jugable
    -- bounds = (mitad del ancho, mitad del alto) → si stageSize = (100, 70), entonces bounds = (50, 35)
    -- Esto define que los robots pueden aparecer entre -50 y +50 en X, y entre -35 y +35 en Y
    let bounds = (fst stageSize / 2, snd stageSize / 2)
    
    -- Generar posiciones aleatorias para cada robot usando la misma semilla pero IDs diferentes
    -- Cada robot obtiene una posición única dentro del área definida por bounds
    let pos1 = generatePositionFromSeed bounds seedBase 1  -- Posición para robot ID 1
    let pos2 = generatePositionFromSeed bounds seedBase 2  -- Posición para robot ID 2
    let pos3 = generatePositionFromSeed bounds seedBase 3  -- Posición para robot ID 3
    
    let robots = Map.fromList
            [ 
                (1, createBasicRobot pos1 "aggressive" 1),
                (2, createBasicRobot pos2 "defensive" 2),
                (3, createBasicRobot pos3 "stupid" 3)
            ]
    backgroundImage <- loadBackgroundImage "background.jpg"
    let initialState = GameState 
                            { 
                                gameWindowSize = window,
                                gameRobots = robots,
                                gameProjectiles = Map.fromList [],
                                gameTime = 0,
                                gameFrame = 0,
                                gameExplosions = Map.fromList [],
                                gameStageSize = (100, 70),
                                gameBackground = backgroundImage,
                                gameKeysPressed = Set.empty,
                                gameTotalProjectileCount = 0,
                                gameTotalExplosionCount = 0,
                                gameSimulationSpeed = 1,
                                gameDebugInfo = True,
                                gamePaused = False
                            }
    playGame initialState