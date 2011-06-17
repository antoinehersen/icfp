module BasicDirections where
import ParseMsg
import SteeringUtils

-- Converges to (0, 0), regardless of obstacles reported by the Telemetry
naiveSteering :: Message -> Steering
naiveSteering (Telemetry _ _ _ x y dir _ objs) =
    let myVec = angleToVec dir in
    let toHome = Vec2 (-x) (-y) in
    case whereIs myVec toHome of
      ToTheLeft -> LeftAndAccel
      ToTheRight -> RightAndAccel

-- Tells is the second vector is orientated to the left or the right of the first one
whereIs :: Vec2 -> Vec2 -> WhereIs
whereIs (Vec2 a1 a2) (Vec2 b1 b2) =
    let crossProdZ = (a1 * b2) - (a2 * b1) in
    if crossProdZ < 0
       then ToTheRight
       else ToTheLeft