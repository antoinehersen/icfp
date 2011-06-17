module Test where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Control.Concurrent.Chan
import ParseMsg

goGlut ch ini = do
	(progname,_) <- getArgsAndInitialize
	createWindow "Hello World"
	displayCallback $= (display ch)
	idleCallback $= Just (updateScreen ch)
	projection (-150) 150 (-150) 150 (-150) 150
	mainLoop


projection xl xu yl yu zl zu = do
  matrixMode $= Projection
  loadIdentity
  ortho xl xu yl yu zl zu
  matrixMode $= Modelview 0


updateScreen ch = do
--  putStrLn "idleing"
  msg <- readChan ch
  mapM_ renderObj msg
  color $ Color3 0 1 (1 :: GLfloat)
  --renderObj $ Boulder 0.1 0.1 0.3
  --cube (5 :: GLfloat)
  flush
--  postRedisplay Nothing

circle	:: [(GLfloat,GLfloat,GLfloat)]
circle	= map (\k -> (sin(2*pi*k/10),cos(2*pi*k/10),0.0)) [1..10]

circleAt x y r = preservingMatrix $ do
	translate $ Vector3 x y 0
	scale r r r
	renderPrimitive Polygon $ do
		mapM_ (\(x, y, z)->vertex$Vertex3 x y z) circle

display ch = do
  putStrLn "Updating"
  clear [ColorBuffer]
  return ()

renderObj (Boulder x y r) = circleAt x y r
renderObj _ = return ()

{-renderObj (Boulder x y r ) = renderPrimitive Points $ do
		              vertex $ Vertex3 x y 0
renderObj a = renderPrimitive Points $ do
		vertex $ Vertex3 0 0 (0 :: GLfloat)-}

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