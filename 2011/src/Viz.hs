module Viz (initGL) where

import Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLUT as GLUT

import Control.Concurrent.MVar
import Interpreter

drawBar :: Float -> Float -> IO ()
drawBar side val = do
  let adj =  val / 65535.0 * 300
  if val < 1 then
      do GL.color $ (GL.Color4 1 0 0 0.9 :: GL.Color4 Float)
         rect (5*side)
  else
      do GL.color $ (GL.Color4 0.3 0 1 0.9 :: GL.Color4 Float)
         rect (adj*side)

  GL.translate (GL.Vector3 5 0 0 ::GL.Vector3 Float)
      where
        t = 5
        rect h = GL.renderPrimitive GL.Quads $ do
                   GL.vertex (GL.Vertex2 0 0 ::GL.Vertex2 Float)
                   GL.vertex (GL.Vertex2 0 h ::GL.Vertex2 Float)
                   GL.vertex (GL.Vertex2 t h ::GL.Vertex2 Float)
                   GL.vertex (GL.Vertex2 t 0 ::GL.Vertex2 Float)


drawBars side ls = do
  GL.preservingMatrix $
    mapM_ (drawBar side ) ls
  return ()

render worldM = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer ]
  GL.matrixMode $= GL.Modelview 0
  GL.loadIdentity

  World propo oppo <- takeMVar worldM

  drawBars 1    $ map fromIntegral $ getVitality propo
  drawBars (-1) $ map fromIntegral $ getVitality oppo

  GL.color $ (GL.Color4 1 1 1 1 :: GL.Color4 Float)
  GL.renderPrimitive GL.Lines $ do
              GL.vertex (GL.Vertex2 0 0 ::GL.Vertex2 Float)
              GL.vertex (GL.Vertex2 1280 0 ::GL.Vertex2 Float)

  GL.flush
  GLUT.swapBuffers

initGL worldM = do
  GLUT.initialDisplayMode $= [GLUT.RGBAMode
                             ,GLUT.Multisampling
                             ,GLUT.DoubleBuffered
                             ,GLUT.WithAlphaComponent
                             ,GLUT.WithDepthBuffer]
  GLUT.initialWindowSize $= GL.Size 800 600
  (name, args) <- GLUT.getArgsAndInitialize
  window <- GLUT.createWindow "icfp 2011"
  GLUT.displayCallback $= render worldM

  GL.clearColor $= GL.Color4 0 0 0 1
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.lineSmooth $= GL.Enabled
  GL.pointSmooth $= GL.Enabled
  GL.polygonSmooth $= GL.Enabled

  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.ortho 0 1280 (-300) 300 (-1) 1

  GLUT.mainLoop


-- main = do putStrLn "Hello"
--           initGL