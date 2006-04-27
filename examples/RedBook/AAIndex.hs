{-
   AAIndex.hs (adapted from aaindex.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2006 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program draws shows how to draw anti-aliased lines in color index
   mode. It draws two diagonal lines to form an X; when 'r' is typed in the
   window, the lines are rotated in opposite directions.
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

rampSize, ramp1Start, ramp2Start :: GLint
rampSize = 16
ramp1Start = 32
ramp2Start = 48

-- Initialize antialiasing for color index mode, including loading a green color
-- ramp starting at ramp1Start, and a blue color ramp starting at ramp2Start.
-- The ramps must be a multiple of 16.
myInit :: IO () 
myInit = do
   flip mapM_ [ 0 .. rampSize - 1 ] $ \i -> do
      let shade = fromIntegral i / fromIntegral rampSize
      colorMapEntry (Index1 (ramp1Start + i)) $= Color3 0 shade 0
      colorMapEntry (Index1 (ramp2Start + i)) $= Color3 0 0 shade

   lineSmooth $= Enabled
   hint LineSmooth $= DontCare
   lineWidth $= 1.5

   clearIndex $= Index1 (fromIntegral ramp1Start)

-- Draw 2 diagonal lines to form an X
display :: State -> DisplayCallback
display state = do
   r <- get (rotAngle state)
   clear [ ColorBuffer ]

   -- resolve overloading, not needed in "real" programs
   let vertex2f = vertex :: Vertex2 GLfloat -> IO ()

   index (Index1 ramp1Start)
   preservingMatrix $ do
      rotate (-(fromIntegral r :: GLfloat)) (Vector3 0 0 0.1)
      renderPrimitive Lines $ do
         vertex2f (Vertex2 (-0.5) 0.5)
         vertex2f (Vertex2 0.5 (-0.5))

   index (Index1 ramp2Start)
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
-- color index display mode, and handle input events.
main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, IndexMode ]
   initialWindowSize $= Size 200 200
   createWindow progName
   state <- makeState
   myInit
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just (keyboard state)
   displayCallback $= display state
   mainLoop
