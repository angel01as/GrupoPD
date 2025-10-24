import Game
import Robot (createBasicRobot)
import GameState
import RandomUtils (generatePositionFromSeed)

import qualified Data.Map as Map (Map, fromList, insert, empty)
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (getPOSIXTime)

import Graphics.Gloss(Picture)
import Graphics.Gloss.Juicy(loadJuicy)

import Rendering(usedImages)
import Data.Default
import UIButton

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
    imagesMap <- loadImages usedImages
    let initialState = def 
                            { 
                                gameWindowSize = window,
                                gameRobots = robots,
                                gameStageSize = (100, 70),
                                gameImages = imagesMap,
                                gameSeed = seedBase,
                                gameButtons = makeButtons
                            }
    playGame initialState

-- Carga varias imágenes y devuelve un diccionario con las rutas como clave
loadImages :: [String] -> IO (Map.Map String Picture)
loadImages [] = pure Map.empty -- Como IO es una mónada hay que envolver los valores
loadImages (p:ps) = do
  maybePic <- loadJuicy p
  rest <- loadImages ps
  case maybePic of
    Just pic -> pure $ Map.insert p pic rest
    Nothing  -> do
      putStrLn $ "[X] No se pudo cargar: " ++ p
      pure rest

makeButtons :: [UIButton GameState]
makeButtons =
  (playButton) : concatMap rowButtons [(-1) .. 1] 
  where
    buttonColumnCenter = 0.25
    buttonOffsetX = 0.075
    btnSize = (0.10, 0.10)
    rowButtons i =
        [ UIButton
            {   
                buttonPosition = (buttonColumnCenter - buttonOffsetX, y),
                buttonSize     = btnSize,
                buttonText     = "-",
                buttonHandler  = getLeftHandler i
            }
        , UIButton
            {   
                buttonPosition = (buttonColumnCenter + buttonOffsetX, y),
                buttonSize     = btnSize,
                buttonText     = "+",
                buttonHandler  = getRightHandler i
            }
        ]
        where
            y = fromIntegral i * 0.25

            getLeftHandler :: Int -> (GameState -> GameState)
            getLeftHandler i
                | i == 1 = (\gs -> if gameTotalRobotCount gs > 0 then gs { gameTotalRobotCount = gameTotalRobotCount gs - 1 } else gs)
                | i == 0 = (\gs -> if gameSimulationSpeed gs > 0 then gs { gameSimulationSpeed = gameSimulationSpeed gs - 0.1 } else gs)
                | i == -1 = changeDebugMode
                | otherwise = id
            getRightHandler :: Int -> (GameState -> GameState)
            getRightHandler i
                | i == 1 = (\gs -> if gameTotalRobotCount gs < 20 then gs { gameTotalRobotCount = gameTotalRobotCount gs + 1 } else gs)
                | i == 0 = (\gs -> if gameSimulationSpeed gs < 10 then gs { gameSimulationSpeed = gameSimulationSpeed gs + 0.1 } else gs)
                | i == -1 = changeDebugMode
                | otherwise = id

            changeDebugMode gs = gs { gameDebugInfo = not (gameDebugInfo gs) }
    playButton = UIButton 
        {
            buttonPosition = (0, -0.7),
            buttonSize     = (0.8, 0.2),
            buttonText     = "Jugar",
            buttonHandler  = (\gs -> gs { gameIsInMenu = False })
        }