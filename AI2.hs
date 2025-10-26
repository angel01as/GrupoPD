module AI2 where

import Robot
import Entities
import Geometry
import Data.Map
import Data.Maybe
import Data.List
import Prelude
import GameState
import Debug.Trace

-- ============================================================================
-- DSL PARA ACCIONES DEL BOT
-- ============================================================================

move :: Robot -> Scalar -> Robot
move robot v = robot {robotVelocity = updatedVelocity}
    where 
        (normal,angulo) = comp2Polar (robotVelocity robot)
        updNormal = normal + v
        updatedVelocity = polar2Comp updNormal angulo

rotate :: Robot -> Angle -> Robot
rotate robot angle = robot {robotVelocity = updatedVelocity, robotOrientation = updatedAngle}
    where
        anguloIn = robotOrientation robot
        updatedAngle = anguloIn + angle
        velocidadIn = robotVelocity robot
        updatedVelocity = rotateVector velocidadIn angle
