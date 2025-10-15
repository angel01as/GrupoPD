import Game
import Robot (createBasicRobot)

-- Punto de entrada al juego.

main :: IO()
main = do
  -- Estado inicial del torneo: dos robots básicos y sin proyectiles
  let window = (1000, 700)
  let robots =
        [ createBasicRobot (-5, 0) "aggressive"
        , createBasicRobot ( 5, 0) "defensive"
        ]
  
  -- Intentar cargar imagen de fondo (opcional)
  backgroundImage <- loadBackgroundImage "background.jpg"  -- Cambia por tu imagen
  let initialState = GS { gsWindowSize = window
                        , gsRobots = robots
                        , gsProjectiles = []
                        , gsTime = 0
                        , gsBackground = backgroundImage }
  playGame initialState