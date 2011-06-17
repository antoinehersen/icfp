module Viz where

-- import Debug.Trace

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Control.Concurrent.Chan
import Data.IORef

import ParseMsg
import SteeringUtils
import CraterDirection

goViz init telChan = do
  let (Init dx dy _ maxSens minSen maxSpeed _ _) = init
  (progname,_) <- getArgsAndInitialize
  initialWindowSize $= Size 680 680
  createWindow "Hello World"
  pathRef <- newIORef (vertex ( Vertex3 dx dy 0))
  displayCallback $= displayTest telChan pathRef maxSpeed
  idleCallback $= Just updateScreen
  projection (-dx/2) (dx/2) (-dy/2)  (dy/2) (-100) 100
  mainLoop

-- renderPrimitive Points do
--  vertex Vertex3 ...
--  vertex Vertex3 ...
-- LineStrip


-- goGlut ch ini = do
-- 	(progname,_) <- getArgsAndInitialize
-- 	createWindow "Hello World"
-- 	displayCallback $= (display ch)
-- 	idleCallback $= Just (updateScreen ch)
-- 	projection (-150) 150 (-150) 150 (-150) 150
-- 	mainLoop


makePath pathRef (Telemetry _ _ _ (x, y) _ speed _ )  maxSpeed = do
  curentPath <- readIORef pathRef
  let fx = realToFrac x
  let fy = realToFrac y
  let col = realToFrac ( speed / maxSpeed )
  let newPath = curentPath >> (vertex $ Vertex3 fx fy (0::GLfloat)) >> (color $ Color3 0 (0.5 :: GLfloat) col )
  writeIORef pathRef newPath
  return newPath
makePath pathRef (BoulderCrash _ ) _ =
    readIORef pathRef
makePath pathRef _  _ = do
  let empty = return ()
  writeIORef pathRef empty
  return empty



projection xl xu yl yu zl zu = do
  matrixMode $= Projection
  loadIdentity
  ortho xl xu yl yu zl zu
  matrixMode $= Modelview 0



displayTest telChan pathRef maxSpeed= do
  tel <- readChan telChan
  clear [ColorBuffer]
-- center
  color $ Color3 0 1 (0 :: GLfloat)
  circleAt (0::GLfloat) 0 (5::GLfloat)
  renderRobot tel
  renderObjs tel
  line <- makePath pathRef tel maxSpeed
  renderPrimitive LineStrip line
  mapM_ showStrategy (justAHead tel)
  flush


updateScreen = do
  postRedisplay Nothing

renderRobot (Telemetry _ _ turn (x, y) dir v _) = do
  color $ Color3 1 1 (0 :: GLfloat)
  preservingMatrix $ do
    translate $ Vector3 x y 10
    rotate dir (Vector3 0 0 1)
    renderPrimitive Points $ do
                       vertex $ Vertex3 0 0 (10::GLfloat)
    renderPrimitive Lines $ do
                       vertex $ Vertex3 0 0 (0::GLfloat)
                       vertex $ Vertex3 v 0 0
    renderPrimitive Lines $ do
                       vertex $ Vertex3 0 0 (0::GLfloat)
                       vertex $ turnLine turn
  color $ Color3 0 1 (0 :: GLfloat)
    where
      (dx, dy) = angleToVec dir
renderRobot _ = do return ()

turnLine NormalLeft = Vertex3 0 3 (0::GLfloat)
turnLine HardLeft = Vertex3 0 6 (0::GLfloat)
turnLine NormalRight = Vertex3 0 (-3) (0::GLfloat)
turnLine HardRight = Vertex3 0 (-6) (0::GLfloat)
turnLine Straight = Vertex3 0 0 (0::GLfloat)



circle	:: [(GLfloat,GLfloat,GLfloat)]
circle	= map (\k -> (sin(2*pi*k/10),cos(2*pi*k/10),0.0)) [1..10]

circleAt x y r = preservingMatrix $ do
	translate $ Vector3 x y 0
	scale r r r
	renderPrimitive Polygon $ do
		mapM_ (\(x, y, z)->vertex$Vertex3 x y z) circle




--showStrategy ( Vec2 ,Strategy ) -> IO
showStrategy ( (x,y) , _ ) = renderPrimitive Points $ do
                               vertex $ Vertex3 xa ya (15::GLfloat)
    where xa = realToFrac x
          ya = realToFrac y

renderObj (Obj objType (x,y) r ) =
    do
      colorObj objType
      circleAt x y r
renderObj (Martian (x,y) _ _ ) =
    do
      color $ Color3 1 0.5 (0 :: GLfloat)
      circleAt x y (pseudoMartianSize)

colorObj Boulder =  color $ Color3 0.5 1 (0 :: GLfloat)
colorObj Crater =  color $ Color3 1 0.2 (0 :: GLfloat)
colorObj Home =  color $ Color3 1 1 (0 :: GLfloat)

renderObjs (Telemetry _ _ _ _ _ _ objs) =
    do mapM_ renderObj ( sizeChange objs ) --- !!! Change the objects size
renderObjs _ = do return()

cube w = do
	renderPrimitive Quads $ do
		vertex $ Vertex3 w w w
		vertex $ Vertex3 w w (-w)
		vertex $ Vertex3 w (-w) (-w)
		vertex $ Vertex3 w (-w) w
		vertex $ Vertex3 w w w
		vertex $ Vertex3 w w (-w)
		vertex $ Vertex3 (-w) w (-w)
		vertex $ Vertex3 (-w) w w
		vertex $ Vertex3 w w w
		vertex $ Vertex3 w (-w) w
		vertex $ Vertex3 (-w) (-w) w
		vertex $ Vertex3 (-w) w w
		vertex $ Vertex3 (-w) w w
		vertex $ Vertex3 (-w) w (-w)
		vertex $ Vertex3 (-w) (-w) (-w)
		vertex $ Vertex3 (-w) (-w) w
		vertex $ Vertex3 w (-w) w
		vertex $ Vertex3 w (-w) (-w)
		vertex $ Vertex3 (-w) (-w) (-w)
		vertex $ Vertex3 (-w) (-w) w
		vertex $ Vertex3 w w (-w)
		vertex $ Vertex3 w (-w) (-w)
		vertex $ Vertex3 (-w) (-w) (-w)
		vertex $ Vertex3 (-w) w (-w)