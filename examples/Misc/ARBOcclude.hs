{-
   ARBOcclude.hs (adapted from arbocclude.c which is (c) Brian Paul)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE
-}

import Control.Monad ( unless, when )
import Data.IORef ( IORef, newIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess), exitFailure )
import Graphics.UI.GLUT

data State = State {
   anim     :: IORef Bool,
   xPos     :: IORef GLfloat,
   sign     :: IORef GLfloat,
   lastTime :: IORef Int }

makeState :: IO State
makeState = do
   a <- newIORef True
   x <- newIORef 0
   s <- newIORef 1
   l <- newIORef =<< get elapsedTime
   return $ State { anim = a, xPos = x, sign = s, lastTime = l }

petrol, orange, white :: Color3 GLfloat
petrol = Color3 0.0 0.6 0.8
orange = Color3 0.8 0.5 0.0
white  = Color3 1.0 1.0 1.0

printString :: Vertex2 GLfloat -> String -> IO ()
printString pos s = do
   color white
   rasterPos pos
   renderString Fixed8By13 s

idle :: State -> IdleCallback
idle state = do
   time <- get elapsedTime
   l <- get (lastTime state)
   let timeDiff = fromIntegral (time - l)

   when (timeDiff >= 20) $ do -- 50Hz update
      lastTime state $= time   

      s <- get (sign state)
      step state (timeDiff / 1000 * s)
      x <- get (xPos state)

      when (x > 2.5) $ do
         xPos state $= 2.5
         sign state $= (-1)

      when (x < -2.5) $ do
         xPos state $= (-2.5)
         sign state $= 1

display :: QueryObject -> State -> DisplayCallback
display occQuery state = do
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
      x <- get (xPos state)
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

keyboard :: State -> KeyboardMouseCallback
keyboard _     (Char '\27')          Down _ _ = exitWith ExitSuccess
keyboard state (Char ' ')            Down _ _ = do anim state $~ not
                                                   setIdleCallback state
keyboard state (SpecialKey KeyLeft)  Down _ _ = step state (-0.1)
keyboard state (SpecialKey KeyRight) Down _ _ = step state   0.1
keyboard _     _                     _    _ _ = return ()

setIdleCallback :: State -> IO ()
setIdleCallback state = do
   a <- get (anim state)
   idleCallback $= if a then Just (idle state) else Nothing

step :: State -> GLfloat -> IO ()
step state s = do
   xPos state $~ (+ s)
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
   state <- makeState
   reshapeCallback $= Just (\size -> viewport $= (Position 0 0, size))
   keyboardMouseCallback $= Just (keyboard state)
   setIdleCallback state
   [ occQuery ] <- genObjectNames 1
   displayCallback $= display occQuery state
   myInit
   mainLoop
