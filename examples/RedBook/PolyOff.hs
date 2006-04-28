{-
   PolyOff.hs (adapted from polyoff.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2006 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates polygon offset to draw a shaded polygon and its
   wireframe counterpart without ugly visual artifacts ("stitching").
-}

import Control.Monad ( when )
import Data.IORef ( IORef, newIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess), exitFailure )
import Graphics.UI.GLUT

data State = State {
   spinX, spinY :: IORef GLfloat,
   tDist :: IORef GLfloat,
   polyFactor :: IORef GLfloat,
   polyUnits :: IORef GLfloat
 }

makeState :: IO State
makeState = do
   x <- newIORef 0
   y <- newIORef 0
   t <- newIORef 0
   f <- newIORef 1
   u <- newIORef 1
   return $ State { spinX = x, spinY = y, tDist = t, polyFactor = f, polyUnits = u }

-- display draws two spheres, one with a gray, diffuse material, the other
-- sphere with a magenta material with a specular highlight.
display :: State -> DisplayList -> DisplayCallback
display state sphereList = do
   clear [ ColorBuffer, DepthBuffer ]
   preservingMatrix $ do
      t <- get (tDist state)
      translate (Vector3 0 0 t)
      x <- get (spinX state)
      rotate x (Vector3 1 0 0)
      y <- get (spinY state)
      rotate y (Vector3 0 1 0)

      materialAmbientAndDiffuse Front $= Color4 0.8 0.8 0.8 1
      materialSpecular Front $= Color4 0 0 0 1
      materialShininess Front $= 0
      lighting $= Enabled
      light (Light 0) $= Enabled
      polygonOffsetFill $= Enabled
      f <- get (polyFactor state)
      u <- get (polyUnits state)
      polygonOffset $= (f, u)
      callList sphereList
      polygonOffsetFill $= Disabled

      lighting $= Disabled
      light (Light 0) $= Disabled
      color (Color3 1 1 (1 :: GLfloat))
      polygonMode $= (Line, Line)
      callList sphereList
      polygonMode $= (Fill, Fill)

   flush

-- specify initial properties
-- create display list with sphere  
-- initialize lighting and depth buffer
gfxinit :: IO DisplayList
gfxinit = do
   clearColor $= Color4 0 0 0 1

   sphereList <- defineNewList Compile $
      renderObject Solid (Sphere' 1 20 12)

   depthFunc $= Just Less

   ambient (Light 0) $= Color4 0 0 0 1
   diffuse (Light 0) $= Color4 1 1 1 1
   specular (Light 0) $= Color4 1 1 1 1
   position (Light 0) $= Vertex4 1 1 1 0
   lightModelAmbient $= Color4 0.2 0.2 0.2 1

   return sphereList

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   perspective 45 (fromIntegral w / fromIntegral h) 1 10
   matrixMode $= Modelview 0
   loadIdentity
   lookAt (Vertex3 0 0 5) (Vertex3 0 0 0) (Vector3 0 1 0)

incSpin :: IORef GLfloat -> IO ()
incSpin spinRef = do
   let wrap n s = if s > n then s - n else s
   spinRef $~ (wrap 360 . (+ 5))
   postRedisplay Nothing

incDist :: State -> GLfloat -> IO ()
incDist state inc = do
   newDist <- fmap (+ inc) $ get (tDist state)
   when (-5 <= newDist && newDist <= 4) $ do
      tDist state $= newDist
      postRedisplay Nothing

incPoly :: String -> IORef GLfloat -> GLfloat -> IO ()
incPoly name polyRef inc = do
   polyRef $~ (+ inc)
   p <- get polyRef
   putStrLn (name ++ " is " ++ show p)
   postRedisplay Nothing

keyboardMouse :: State -> KeyboardMouseCallback
keyboardMouse state k Down _ _ = case k of
   (MouseButton LeftButton)   -> incSpin (spinX state)
   (MouseButton MiddleButton) -> incSpin (spinY state)
   (MouseButton RightButton)  -> exitWith ExitSuccess
   (Char 't')                 -> incDist state 0.5
   (Char 'T')                 -> incDist state (-0.5)
   (Char 'F')                 -> incPoly "polyFactor" (polyFactor state) 0.1
   (Char 'f')                 -> incPoly "polyFactor" (polyFactor state) (-0.1)
   (Char 'U')                 -> incPoly "polyUnits"  (polyUnits  state) 1
   (Char 'u')                 -> incPoly "polyUnits"  (polyUnits  state) (-1)
   _                          -> return ()
keyboardMouse _ _ _ _ _ = return ()

-- Main Loop: Open window with initial window size, title bar, RGBA display
-- mode, and handle input events.
main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode, WithDepthBuffer ]
   createWindow progName

   -- we have to do this *after* createWindow, otherwise we have no OpenGL context
   version <- get glVersion
   when (take 3 version == "1.0") $ do
      putStrLn "This program demonstrates a feature which is not in OpenGL Version 1.0."
      putStrLn "If your implementation of OpenGL Version 1.0 has the right extensions,"
      putStrLn "you may be able to modify this program to make it run."
      exitFailure

   state <- makeState
   sphereList <- gfxinit
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just (keyboardMouse state)
   displayCallback $= display state sphereList
   mainLoop
