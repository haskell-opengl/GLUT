{-
   AARGB.hs (adapted from aargb.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2006 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program draws shows how to draw anti-aliased lines. It draws two
   diagonal lines to form an X; when 'r' is typed in the window, the lines are
   rotated in opposite directions.
-}

import Data.Char ( toLower )
import Data.IORef ( IORef, newIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

data State = State { rotAngle :: IORef Int }

makeState :: IO State
makeState = do
   r <- newIORef 0
   return $ State { rotAngle = r }

-- Initialize antialiasing for RGBA mode, including alpha blending, hint, and
-- line width. Print out implementation specific info on line width granularity
-- and width.
myInit :: IO () 
myInit = do
   g <- get smoothLineWidthGranularity
   putStrLn ("smoothLineWidthGranularity is " ++ show g)

   r <- get smoothLineWidthRange 
   putStrLn ("smoothLineWidthRange is " ++ show r)

   lineSmooth $= Enabled

   blend $= Enabled
   blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
   hint LineSmooth $= DontCare
   lineWidth $= 1.5

   clearColor $= Color4 0 0 0 0

-- Draw 2 diagonal lines to form an X
display :: State -> DisplayCallback
display state = do
   r <- get (rotAngle state)
   clear [ ColorBuffer ]

   -- resolve overloading, not needed in "real" programs
   let color3f = color :: Color3 GLfloat -> IO ()
       vertex2f = vertex :: Vertex2 GLfloat -> IO ()

   color3f (Color3 0 1 0)
   preservingMatrix $ do
      rotate (-(fromIntegral r :: GLfloat)) (Vector3 0 0 0.1)
      renderPrimitive Lines $ do
         vertex2f (Vertex2 (-0.5) 0.5)
         vertex2f (Vertex2 0.5 (-0.5))

   color3f (Color3 0 0 1)
   preservingMatrix $ do
      rotate (fromIntegral r :: GLfloat) (Vector3 0 0 0.1)
      renderPrimitive Lines $ do
         vertex2f (Vertex2 0.5 0.5)
         vertex2f (Vertex2 (-0.5) (-0.5))

   flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   let wf = fromIntegral w
       hf = fromIntegral h
   if w <= h
      then ortho2D (-1) 1 (-1*hf/wf) (1*hf/wf)
      else ortho2D (-1*wf/hf) (1*wf/hf) (-1) 1
   matrixMode $= Modelview 0
   loadIdentity

keyboard :: State -> KeyboardMouseCallback
keyboard state (Char c) Down _ _ = case toLower c of
   'r'   -> do rotAngle state  $~ ((`mod` 360) . (+ 30)); postRedisplay Nothing
   '\27' -> exitWith ExitSuccess
   _     -> return ()
keyboard _ _ _ _ _ = return ()

-- Main Loop
-- Open window with initial window size, title bar, 
-- RGBA display mode, and handle input events.
main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode ]
   initialWindowSize $= Size 200 200
   createWindow progName
   state <- makeState
   myInit
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just (keyboard state)
   displayCallback $= display state
   mainLoop
