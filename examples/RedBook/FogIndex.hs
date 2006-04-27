{-
   FogIndex.hs (adapted from fogindex.c which is (c) Silicon Graphics, Inc)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program draws 5 wireframe spheres, each at a different z distance from
   the eye, in linear fog.
-}

import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

-- Initialize color map and fog. Set screen clear color to end of color ramp.
numColors, rampStart :: GLint
numColors = 32
rampStart = 16

myInit :: IO ()
myInit = do
   depthFunc $= Just Less

   flip mapM_ [ 0 .. numColors - 1 ] $ \i -> do
      let shade = fromIntegral (numColors - i) / fromIntegral numColors
      colorMapEntry (Index1 (rampStart + i)) $= Color3 shade shade shade
   fog $= Enabled

   fogMode $= Linear 1 6
   fogIndex $= Index1 numColors
   hint Fog $= Nicest
   clearIndex $= Index1 (fromIntegral (numColors + rampStart - 1))

renderSpehere :: Vector3 GLfloat -> IO ()
renderSpehere xyz =
   preservingMatrix $ do
      translate xyz
      renderObject Wireframe (Sphere' 0.4 16 16)

-- display draws 5 spheres at different z positions.
display :: DisplayCallback
display = do
   clear [ ColorBuffer, DepthBuffer ]
   index (Index1 rampStart)
   mapM_ renderSpehere [ Vector3 x (-0.5) (-3 - x) | x <- [-2 .. 2] ]
   flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   let wf = fromIntegral w
       hf = fromIntegral h
   if w <= h
      then ortho (-2.5) 2.5 (-2.5*hf/wf) (2.5*hf/wf) (-10.0) 10.0
      else ortho (-2.5*wf/hf) (2.5*wf/hf) (-2.5) 2.5 (-10.0) 10.0
   matrixMode $= Modelview 0
   loadIdentity

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _ _ _ _ = return ()

-- Main Loop: Open window with initial window size, title bar, color index
-- display mode, depth buffer, and handle input events.
main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, IndexMode, WithDepthBuffer ]
   initialWindowSize $= Size 500 500
   createWindow progName
   myInit
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just keyboard
   displayCallback $= display
   mainLoop
