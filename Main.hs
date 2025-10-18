import Game
import Robot (createBasicRobot)
import GameState

import qualified Data.Map as Map (fromList)
import qualified Data.Set as Set

-- Punto de entrada al juego.

main :: IO()
main = do
    -- Estado inicial del torneo: dos robots básicos y sin proyectiles
    let window = (1000, 700)
    let robots = Map.fromList
            [ 
                (1, createBasicRobot (-10, 0) "aggressive" 1),
                (2, createBasicRobot ( 15, 0) "stupid" 2)
            ]
    backgroundImage <- loadBackgroundImage "background.jpg"
    let initialState = GameState 
                            { 
                                gameWindowSize = window,
                                gameRobots = robots,
                                gameProjectiles = Map.fromList [],
                                gameTime = 0,
                                gameExplosions = Map.fromList [],
                                gameStageSize = (100, 70),
                                gameBackground = backgroundImage,
                                gameKeysPressed = Set.empty,
                                gameTotalProjectileCount = 0,
                                gameTotalExplosionCount = 0
                            }
    playGame initialState