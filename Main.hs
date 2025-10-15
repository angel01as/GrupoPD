import Game
import Robot (createBasicRobot)

-- Punto de entrada al juego.

main :: IO()
main = do
  -- Estado inicial del torneo: dos robots básicos y sin proyectiles
  let window = (1000, 700)
  let robots =
        [ createBasicRobot (-10, 0) "aggressive"
        , createBasicRobot ( 10, 0) "defensive"
        ]
  let initialState = GS { gsWindowSize = window
                        , gsRobots = robots
                        , gsProjectiles = []
                        , gsTime = 0 }
  playGame initialState