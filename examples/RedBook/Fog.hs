{-
   Fog.hs (adapted from fog.c which is (c) Silicon Graphics, Inc)
   Copyright (c) Sven Panne 2003 <sven_panne@yahoo.com>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program draws 5 red spheres, each at a different z distance from the
   eye, in different types of fog. Pressing the f key chooses between 3 types
   of fog: exponential, exponential squared, and linear. In this program, there
   is a fixed density value, as well as fixed start and end values for the
   linear fog.
-}

import Data.Char ( toLower )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

myInit :: IO ()
myInit = do
   depthFunc $= Just Less

   position (Light 0) $= Vertex4 0.5 0.5 3.0 0.0
   lighting $= Enabled
   light (Light 0) $= Enabled

   -- NOTE: The alpha values are missing from fog.c!
   materialAmbient   Front $= Color4 0.1745   0.01175  0.01175  1.0
   materialDiffuse   Front $= Color4 0.61424  0.04136  0.04136  1.0
   materialSpecular  Front $= Color4 0.727811 0.626959 0.626959 1.0
   materialShininess Front $= 0.6 * 128

   fog $= Enabled
   let c = Color4 0.5 0.5 0.5 1.0
   fogMode $= Exp 0.35
   fogColor $= c
   hint Fog $= DontCare
   clearColor $= c

renderSpehere :: Vector3 GLfloat -> IO ()
renderSpehere xyz =
   preservingMatrix $ do
      translate xyz
      renderObject Solid (Sphere' 0.4 16 16)

-- display draws 5 spheres at different z positions.
display :: DisplayCallback
display = do
   clear [ ColorBuffer, DepthBuffer ]
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
keyboard (Char c) Down _ _ = case toLower c of
   'f'   -> do
      mode <- get fogMode
      case mode of
         Linear _ _    -> do fogMode $= Exp   0.35; putStrLn "Fog mode is Exp"
         Exp _         -> do fogMode $= Exp2  0.35; putStrLn "Fog mode is Exp2"
         Exp2 _        -> do fogMode $= Linear 1 5; putStrLn "Fog mode is Linear"
      postRedisplay Nothing
   '\27' -> exitWith ExitSuccess
   _     -> return ()
keyboard _ _ _ _ = return ()

-- Main Loop: Open window with initial window size, title bar, RGBA display
-- mode, depth buffer, and handle input events.
main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode, WithDepthBuffer ]
   initialWindowSize $= Size 500 500
   createWindow progName
   myInit
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just keyboard
   displayCallback $= display
   mainLoop
