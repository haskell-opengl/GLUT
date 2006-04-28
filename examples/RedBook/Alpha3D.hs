{-
   Alpha3D.hs (adapted from alpha3D.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2006 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates how to intermix opaque and alpha blended polygons
   in the same scene, by using depthMask. Press the 'a' key to animate moving
   the transparent object through the opaque object.  Press the 'r' key to reset
   the scene.
-}

import Data.Char ( toLower )
import Data.IORef ( IORef, newIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

maxZ, minZ, zInc :: GLfloat
maxZ = 8
minZ = -8
zInc = 0.4

-- We don't animate via the idle callback, because this is way too fast on
-- modern computers. A timer with the delay below is used instead for redraw.
delay :: Timeout
delay = 100

data State = State { solidZ, transparentZ :: IORef GLfloat }

makeState :: IO State
makeState = do
   s <- newIORef maxZ
   t <- newIORef minZ
   return $ State { solidZ = s, transparentZ = t }

data DisplayLists = DisplayLists { sphereList, cubeList :: DisplayList }

myInit :: IO DisplayLists
myInit = do
   materialSpecular Front $= Color4 1 1 1 0.15
   materialShininess Front $= 100
   position (Light 0) $= Vertex4 0.5 0.5 1 0

   lighting $= Enabled
   light (Light 0) $= Enabled
   depthFunc $= Just Less

   s <- defineNewList Compile $ renderObject Solid (Sphere' 0.4 16 16)
   c <- defineNewList Compile $ renderObject Solid (Cube 0.6)
   return $ DisplayLists { sphereList = s, cubeList = c }

display :: State -> DisplayLists -> DisplayCallback
display state displayLists = do
   clear [ ColorBuffer, DepthBuffer ]

   preservingMatrix $ do
      s <- get (solidZ state)
      translate (Vector3 (-0.15) (-0.15) s)
      materialEmission Front $= Color4 0 0 0 1
      materialDiffuse Front $= Color4 0.75 0.75 0 1
      callList (sphereList displayLists)

   preservingMatrix $ do
      t <- get (transparentZ state)
      translate (Vector3 (0.15) (0.15) t)
      rotate (15 :: GLfloat) (Vector3 1 1 0)
      rotate (30 :: GLfloat) (Vector3 0 1 0)
      materialEmission Front $= Color4 0 0.3 0.3 0.6
      materialDiffuse Front $= Color4 0 0.8 0.8 0.6
      blend $= Enabled
      depthMask $= Disabled
      blendFunc $= (SrcAlpha, One)
      callList (cubeList displayLists)
      depthMask $= Enabled
      blend $= Disabled

   swapBuffers

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   let wf = fromIntegral w
       hf = fromIntegral h
   if w <= h
      then ortho (-1.5) 1.5 (-1.5*hf/wf) (1.5*hf/wf) (-10) 10
      else ortho (-1.5*wf/hf) (1.5*wf/hf) (-1.5) 1.5 (-10) 10
   matrixMode $= Modelview 0
   loadIdentity

animate :: State -> TimerCallback
animate state = do
   s <- get (solidZ state)
   t <- get (transparentZ state)
   if (s <= minZ || t >= maxZ)
      then idleCallback $= Nothing
      else do
         solidZ state $~ (+ (- zInc))
         transparentZ state $~ (+ zInc)
         addTimerCallback delay (animate state)
         postRedisplay Nothing

keyboard :: State -> KeyboardMouseCallback
keyboard state (Char c) Down _ _ = case toLower c of
   'a'   -> do solidZ state $= maxZ; transparentZ state $= minZ; addTimerCallback delay (animate state)
   'r'   -> do solidZ state $= maxZ; transparentZ state $= minZ; postRedisplay Nothing
   '\27' -> exitWith ExitSuccess
   _     -> return ()
keyboard _ _ _ _ _ = return ()

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   -- The original C example uses single buffering, which flickers a lot.
   initialDisplayMode $= [ DoubleBuffered, RGBMode, WithDepthBuffer ]
   initialWindowSize $= Size 500 500
   createWindow progName
   state <- makeState
   displayLists <- myInit
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just (keyboard state)
   displayCallback $= display state displayLists
   mainLoop
