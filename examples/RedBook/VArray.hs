{-
   VArray.hs (adapted from varray.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2004 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates vertex arrays. NOTE: This program uses withArray
   in an inefficient way, because some Haskell lists are marshaled more than
   once. This could easily be fixed by doing this at initialization time and
   passing the pointers around, but this would probably make the program a bit
   less clear.
-}

import Control.Monad ( when )
import Data.IORef ( IORef, newIORef, readIORef, modifyIORef )
import Foreign ( withArray )
import System.Exit ( exitFailure, exitWith, ExitCode(..) )
import Graphics.UI.GLUT

data SetupMethod = Pointer | Interleaved
   deriving ( Eq, Bounded, Enum )

data DerefMethod = DrawArray | ArrayElement | DrawElements
   deriving ( Eq, Bounded, Enum )

setup :: IORef SetupMethod -> IO ()
setup setupMethod = do
   s <- readIORef setupMethod
   case s of
      Pointer -> do
         let vertices = [  25,  25,
                          100, 325,
                          175,  25,
                          175, 325,
                          250,  25,
                          325, 325 ] :: [GLint]
             colors = [ 1.0,  0.2,  0.2,
                        0.2,  0.2,  1.0,
                        0.8,  1.0,  0.2,
                        0.75, 0.75, 0.75,
                        0.35, 0.35, 0.35,
                        0.5,  0.5,  0.5 ] :: [GLfloat]
         clientState VertexArray $= Enabled
         clientState ColorArray $= Enabled
         withArray vertices $ \verticesBuf ->
            arrayPointer VertexArray $= VertexArrayDescriptor 2 Int 0 verticesBuf
         withArray colors $ \ colorsBuf ->
            arrayPointer ColorArray $= VertexArrayDescriptor 3 Float 0 colorsBuf
      Interleaved ->
         let intertwined = [ 1.0, 0.2, 1.0, 100.0, 100.0, 0.0,
                             1.0, 0.2, 0.2,   0.0, 200.0, 0.0,
                             1.0, 1.0, 0.2, 100.0, 300.0, 0.0,
                             0.2, 1.0, 0.2, 200.0, 300.0, 0.0,
                             0.2, 1.0, 1.0, 300.0, 200.0, 0.0,
                             0.2, 0.2, 1.0, 200.0, 100.0, 0.0 ] :: [GLfloat]
         in withArray intertwined $ interleavedArrays C3fV3f 0

myInit :: IORef SetupMethod -> IO () 
myInit setupMethod = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Smooth
   setup setupMethod

display :: IORef DerefMethod -> DisplayCallback
display derefMethod = do
   clear [ ColorBuffer ]
   d <- readIORef derefMethod
   case d of
      DrawArray    -> drawArrays Triangles 0 6
      ArrayElement -> renderPrimitive Triangles $ mapM_ arrayElement [ 2, 3, 5 ]
      DrawElements -> let indices = [ 0, 1, 3, 4 ] :: [GLuint]
                      in withArray indices $ drawElements Polygon 4 UnsignedInt
   flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   ortho2D 0 (fromIntegral w) 0 (fromIntegral h)
   -- the following line is not in the original example, but it's good style...
   matrixMode $= Modelview 0

keyboardMouse :: IORef SetupMethod -> IORef DerefMethod -> KeyboardMouseCallback
keyboardMouse setupMethod _ (MouseButton LeftButton) Down _ _ = do
   modifyIORef setupMethod nextValue
   setup setupMethod
   postRedisplay Nothing
keyboardMouse _ derefMethod (MouseButton _) Down _ _ = do
   modifyIORef derefMethod nextValue
   postRedisplay Nothing
keyboardMouse _ _ (Char '\27') Down _ _ = exitWith ExitSuccess
keyboardMouse _ _ _ _ _ _ = return ()

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
   setupMethod <- newIORef Pointer
   derefMethod <- newIORef DrawArray
   myInit setupMethod
   displayCallback $= display derefMethod
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just (keyboardMouse setupMethod derefMethod)
   mainLoop
