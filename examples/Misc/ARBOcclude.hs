{-
   ARBOcclude.hs (adapted from arbocclude.c which is (c) Brian Paul)
   Copyright (c) Sven Panne 2002-2004 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE
-}

import Control.Monad ( unless, when )
import Data.IORef ( IORef, newIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess), exitFailure )
import Graphics.UI.GLUT

petrol, orange, white :: Color3 GLfloat
petrol = Color3 0.0 0.6 0.8
orange = Color3 0.8 0.5 0.0
white  = Color3 1.0 1.0 1.0

printString :: Vertex2 GLfloat -> String -> IO ()
printString pos s = do
   color white
   rasterPos pos
   renderString Fixed8By13 s

idle :: IORef GLfloat -> IORef GLfloat -> IORef Int -> IdleCallback
idle xPos sign lastTime = do
   time <- get elapsedTime
   l <- get lastTime
   let timeDiff = fromIntegral (time - l)

   when (timeDiff >= 20) $ do -- 50Hz update
      lastTime $= time   

      s <- get sign
      step xPos (timeDiff / 1000 * s)
      x <- get xPos

      when (x > 2.5) $ do
         xPos $= 2.5
         sign $= (-1)

      when (x < -2.5) $ do
         xPos $= (-2.5)
         sign $= 1

display :: QueryObject -> IORef GLfloat -> DisplayCallback
display occQuery xPos = do
   clear [ ColorBuffer, DepthBuffer ]

   matrixMode $= Projection
   loadIdentity
   frustum (-1) 1 (-1) 1 5 25
   matrixMode $= Modelview 0
   loadIdentity
   translate (Vector3 0 0 (-15 :: GLfloat))

   drawOccludingPolygons

   -- draw the test polygon with occlusion testing
   passed <- preservingMatrix $ do
      x <- get xPos
      translate (Vector3 x 0 (-0.5))
      scale 0.3 0.3 (1.0 :: GLfloat)
      rotate (-90 * x) (Vector3 0 0 1)

      withQuery SamplesPassed occQuery $ do
         colorMask $= Color4 Disabled Disabled Disabled Disabled
         depthMask $= Disabled
         drawRect

      p <- waitForResult occQuery

      -- turn off occlusion testing
      colorMask $= Color4 Enabled Enabled Enabled Enabled
      depthMask $= Enabled

      -- draw the orange rect, so we can see what's going on
      color orange
      drawRect

      return p

   -- Print result message
   matrixMode $= Projection
   loadIdentity
   ortho (-1) 1 (-1) 1 (-1) 1
   matrixMode $= Modelview 0
   loadIdentity

   printString (Vertex2 (-0.50) (-0.7))
      (" " ++ flushRight 4 passed ++ " Fragments Visible")
   when (passed == 0) $
      printString (Vertex2 (-0.25) (-0.8)) "Fully Occluded"

   swapBuffers

drawOccludingPolygons :: IO ()
drawOccludingPolygons = do
   color petrol
   drawQuads [
      Vertex2 (-1.6) (-1.5),
      Vertex2 (-0.4) (-1.5),
      Vertex2 (-0.4)   1.5 ,
      Vertex2 (-1.6)   1.5 ,

      Vertex2   0.4  (-1.5),
      Vertex2   1.6  (-1.5),
      Vertex2   1.6    1.5 ,
      Vertex2   0.4    1.5 ]

drawRect :: IO ()
drawRect = do
   drawQuads [
      Vertex2 (-1) (-1),
      Vertex2   1  (-1),
      Vertex2   1    1 ,
      Vertex2 (-1)   1 ]

drawQuads :: [Vertex2 GLfloat] -> IO ()
drawQuads = renderPrimitive Quads . mapM_ vertex

waitForResult :: QueryObject -> IO GLuint
waitForResult occQuery = do
   let loop = do -- do useful work here, if any
                 ready <- get (queryResultAvailable occQuery)
                 unless ready loop
   loop
   get (queryResult occQuery)

flushRight :: Show a => Int -> a -> String
flushRight width x = replicate (width - length s) ' ' ++ s
   where s = show x

keyboard :: IORef Bool -> IORef GLfloat -> IORef GLfloat -> IORef Int -> KeyboardMouseCallback
keyboard _    _    _    _        (Char '\27')          Down _ _ = exitWith ExitSuccess
keyboard anim xPos sign lastTime (Char ' ')            Down _ _ = do anim $~ not
                                                                     setIdleCallback anim xPos sign lastTime
keyboard _    xPos _    _        (SpecialKey KeyLeft)  Down _ _ = step xPos (-0.1)
keyboard _    xPos _    _        (SpecialKey KeyRight) Down _ _ = step xPos   0.1
keyboard _    _    _    _        _                     _    _ _ = return ()

setIdleCallback :: IORef Bool -> IORef GLfloat -> IORef GLfloat -> IORef Int -> IO ()
setIdleCallback anim xPos sign lastTime = do
   a <- get anim
   idleCallback $= if a then Just (idle xPos sign lastTime) else Nothing

step :: IORef GLfloat -> GLfloat -> IO ()
step xPos s = do
   xPos $~ (+ s)
   postRedisplay Nothing

myInit :: IO ()
myInit = do
   exts <- get glExtensions
   unless ("GL_ARB_occlusion_query" `elem` exts) $ do
      putStrLn "Sorry, this demo requires the GL_ARB_occlusion_query extension."
      exitFailure

   bits <- get (queryCounterBits SamplesPassed)
   unless (bits > 0) $ do
      putStrLn "Hmmm, GL_QUERY_COUNTER_BITS_ARB is zero!"
      exitFailure

   depthFunc $= Just Less

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialWindowPosition $= Position 0 0
   initialWindowSize $= Size 400 400
   initialDisplayMode $= [ RGBMode, DoubleBuffered, WithDepthBuffer ]
   createWindow progName
   reshapeCallback $= Just (\size -> viewport $= (Position 0 0, size))
   anim <- newIORef True
   xPos <- newIORef 0
   sign <- newIORef 1
   lastTime <- get elapsedTime >>= newIORef
   keyboardMouseCallback $= Just (keyboard anim xPos sign lastTime)
   setIdleCallback anim xPos sign lastTime
   [ occQuery ] <- genObjectNames 1
   displayCallback $= display occQuery xPos
   myInit
   mainLoop
