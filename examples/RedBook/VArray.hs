{-
   VArray.hs (adapted from varray.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates vertex arrays.
-}

import Control.Monad ( when )
import Data.IORef ( IORef, newIORef )
import Foreign ( Ptr, newArray )
import System.Exit ( exitFailure, exitWith, ExitCode(..) )
import Graphics.UI.GLUT

data SetupMethod = Pointer | Interleaved
   deriving ( Eq, Bounded, Enum )

data DerefMethod = DrawArray | ArrayElement | DrawElements
   deriving ( Eq, Bounded, Enum )

makeVertices :: IO (Ptr (Vertex2 GLint))
makeVertices = newArray [
   Vertex2  25  25,
   Vertex2 100 325,
   Vertex2 175  25,
   Vertex2 175 325,
   Vertex2 250  25,
   Vertex2 325 325 ]

makeColors :: IO (Ptr (Color3 GLfloat))
makeColors = newArray [
   Color3 1.0  0.2  0.2,
   Color3 0.2  0.2  1.0,
   Color3 0.8  1.0  0.2,
   Color3 0.75 0.75 0.75,
   Color3 0.35 0.35 0.35,
   Color3 0.5  0.5  0.5 ]

makeIntertwined :: IO (Ptr GLfloat)
makeIntertwined = newArray [
   1.0, 0.2, 1.0, 100.0, 100.0, 0.0,
   1.0, 0.2, 0.2,   0.0, 200.0, 0.0,
   1.0, 1.0, 0.2, 100.0, 300.0, 0.0,
   0.2, 1.0, 0.2, 200.0, 300.0, 0.0,
   0.2, 1.0, 1.0, 300.0, 200.0, 0.0,
   0.2, 0.2, 1.0, 200.0, 100.0, 0.0 ]

makeIndices :: IO (Ptr GLuint)
makeIndices = newArray [ 0, 1, 3, 4 ]

data State = State {
   vertices    :: Ptr (Vertex2 GLint),
   colors      :: Ptr (Color3 GLfloat),
   intertwined :: Ptr GLfloat,
   indices     :: Ptr GLuint,
   setupMethod :: IORef SetupMethod,
   derefMethod :: IORef DerefMethod }

makeState :: IO State
makeState = do
   v <- makeVertices
   c <- makeColors
   i <- makeIntertwined
   n <- makeIndices
   s <- newIORef Pointer
   d <- newIORef DrawArray
   return $ State { vertices = v, colors = c, intertwined = i,
                    indices = n, setupMethod = s, derefMethod = d }

setup :: State -> IO ()
setup state = do
   s <- get (setupMethod state)
   case s of
      Pointer -> do
         clientState VertexArray $= Enabled
         clientState ColorArray $= Enabled
         arrayPointer VertexArray $= VertexArrayDescriptor 2 Int 0 (vertices state)
         arrayPointer ColorArray $= VertexArrayDescriptor 3 Float 0 (colors state)
      Interleaved ->
         interleavedArrays C3fV3f 0 (intertwined state)

myInit :: State -> IO ()
myInit state = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Smooth
   setup state

display :: State -> DisplayCallback
display state = do
   clear [ ColorBuffer ]
   d <- get (derefMethod state)
   case d of
      DrawArray    -> drawArrays Triangles 0 6
      ArrayElement -> renderPrimitive Triangles $ mapM_ arrayElement [ 2, 3, 5 ]
      DrawElements -> drawElements Polygon 4 UnsignedInt (indices state)
   flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   ortho2D 0 (fromIntegral w) 0 (fromIntegral h)
   -- the following line is not in the original example, but it's good style...
   matrixMode $= Modelview 0

keyboardMouse :: State -> KeyboardMouseCallback
keyboardMouse state (MouseButton LeftButton) Down _ _ = do
   setupMethod state $~ nextValue
   setup state
   postRedisplay Nothing
keyboardMouse state (MouseButton _) Down _ _ = do
   derefMethod state $~ nextValue
   postRedisplay Nothing
keyboardMouse _ (Char '\27') Down _ _ = exitWith ExitSuccess
keyboardMouse _ _ _ _ _ = return ()

nextValue :: (Eq a, Bounded a, Enum a) => a -> a
nextValue x = if x == maxBound then minBound else succ x

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode ]
   initialWindowSize $= Size 350 350
   initialWindowPosition $= Position 100 100
   createWindow progName
   -- we have to do this *after* createWindow, otherwise we have no OpenGL context
   version <- get glVersion
   when (take 3 version == "1.0") $ do
      putStrLn "This program demonstrates a feature which is not in OpenGL Version 1.0."
      putStrLn "If your implementation of OpenGL Version 1.0 has the right extensions,"
      putStrLn "you may be able to modify this program to make it run."
      exitFailure
   state <- makeState
   myInit state
   displayCallback $= display state
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just (keyboardMouse state)
   mainLoop
