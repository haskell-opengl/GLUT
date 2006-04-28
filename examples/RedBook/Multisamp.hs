{-
   Multisamp.hs (adapted from multisamp.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2006 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program draws shows how to use multisampling to draw anti-aliased
   geometric primitives. The same display list, a pinwheel of triangles and
   lines of varying widths, is rendered twice. Multisampling is enabled when the
   left side is drawn. Multisampling is disabled when the right side is drawn.

   Pressing the 'b' key toggles drawing of the checkerboard background.
   Antialiasing is sometimes easier to see when objects are rendered over a
   contrasting background.
-}

import Control.Monad ( when )
import Data.Char ( toLower )
import Data.IORef ( IORef, newIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

data State = State { bgToggle :: IORef Bool }

makeState :: IO State
makeState = do
   b <- newIORef True
   return $ State { bgToggle = b }

data DisplayLists = DisplayLists { pinwheelList, backgroundList :: DisplayList }

-- Print out state values related to multisampling. Create display list with
-- "pinwheel" of lines and triangles.
myInit :: IO DisplayLists
myInit = do
   clearColor $= Color4 0 0 0 0
   sb <- get sampleBuffers
   putStrLn ("number of sample buffers is " ++ show sb)
   s <- get samples
   putStrLn ("number of samples is " ++ show s)

   -- resolve overloading, not needed in "real" programs
   let color3f = color :: Color3 GLfloat -> IO ()
       vertex2f = vertex :: Vertex2 GLfloat -> IO ()

   p <- defineNewList Compile $ do
      flip mapM_ [ 0 .. 18 ] $ \i ->
         preservingMatrix $ do
            rotate (360 * fromIntegral i / 19 :: GLfloat) (Vector3 0 0 1)
            color3f (Color3 1 1 1)
            lineWidth $= fromIntegral ((i `mod` 3 :: Int) + 1)
            renderPrimitive Lines $ do
              vertex2f (Vertex2 0.25 0.05)
              vertex2f (Vertex2 0.9 0.2)
            color3f (Color3 0 1 1)
            renderPrimitive Triangles $ do
               vertex2f (Vertex2 0.25 0)
               vertex2f (Vertex2 0.9 0)
               vertex2f (Vertex2 0.875 0.1)

   b <- defineNewList Compile $ do
      color3f (Color3 1 0.5 0)
      renderPrimitive Quads $
         flip mapM_ [ 0 .. 15 ] $ \i ->
            flip mapM_ [ 0 .. 15 ] $ \j ->
               when (((i + j) `mod` 2 :: Int) == 0) $ do
                  let ii = fromIntegral i * 0.25
                      jj = fromIntegral j * 0.25
                  vertex2f (Vertex2 (-2.0  + ii) (-2.0  + jj))
                  vertex2f (Vertex2 (-2.0  + ii) (-1.75 + jj))
                  vertex2f (Vertex2 (-1.75 + ii) (-1.75 + jj))
                  vertex2f (Vertex2 (-1.75 + ii) (-2.0  + jj))

   return $ DisplayLists { pinwheelList = p, backgroundList = b }

-- Draw two sets of primitives, so that you can compare the user of
-- multisampling against its absence.
--
-- This code enables antialiasing and draws one display list and disables and
-- draws the other display list
display :: State -> DisplayLists -> DisplayCallback
display state displayLists = do
   clear [ ColorBuffer ]

   t <- get (bgToggle state)
   when t $
      callList (backgroundList displayLists)

   -- resolve overloading, not needed in "real" programs
   let translatef = translate :: Vector3 GLfloat -> IO ()

   multisample $= Enabled
   preservingMatrix $ do
      translatef (Vector3 (-1) 0 0)
      callList (pinwheelList displayLists)

   multisample $= Disabled
   preservingMatrix $ do
      translatef (Vector3 1 0 0)
      callList (pinwheelList displayLists)

   swapBuffers

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   let wf = fromIntegral w
       hf = fromIntegral h
   if w <= 2 * h
      then ortho2D (-2) 2 (-2*hf/wf) (2*hf/wf)
      else ortho2D (-2*wf/hf) (2*wf/hf) (-2) 2
   matrixMode $= Modelview 0
   loadIdentity

keyboard :: State -> KeyboardMouseCallback
keyboard state (Char c) Down _ _ = case toLower c of
   'b'   -> do bgToggle state $~ not; postRedisplay Nothing
   '\27' -> exitWith ExitSuccess
   _     -> return ()
keyboard _ _ _ _ _ = return ()

-- Main Loop: Open window with initial window size, title bar, RGBA display
-- mode, and handle input events.
main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ DoubleBuffered, RGBMode, Multisampling ]
   initialWindowSize $= Size 600 300
   createWindow progName
   state <- makeState
   displayLists <- myInit
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just (keyboard state)
   displayCallback $= display state displayLists
   mainLoop
