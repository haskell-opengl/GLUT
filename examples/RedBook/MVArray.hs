{-
   MVArray.hs (adapted from mvarray.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2004 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates multiple vertex arrays, specifically the OpenGL
   routine multiDrawElements. NOTE: This program uses withArray in an
   inefficient way, because some Haskell lists are marshaled more than once.
   This could easily be fixed by doing this at initialization time and passing
   the pointers around, but this would probably make the program a bit less
   clear.
-}

import Control.Monad ( unless )
import Data.List ( genericLength )
import Foreign ( withMany, withArray )
import System.Exit ( exitFailure, exitWith, ExitCode(..) )
import Graphics.UI.GLUT

setupPointer :: IO ()
setupPointer = do
   let vertices = [  25, 25,
                     75, 75,
                    100, 125,
                    150,  75,
                    200, 175,
                    250, 150,
                    300, 125,
                    100, 200,
                    150, 250,
                    200, 225,
                    250, 300,
                    300, 250 ] :: [GLint]
   clientState VertexArray $= Enabled
   withArray vertices $ \verticesBuf ->
      arrayPointer VertexArray $= VertexArrayDescriptor 2 Int 0 verticesBuf

myInit :: IO () 
myInit = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Smooth
   setupPointer

display :: DisplayCallback
display = do
   clear [ ColorBuffer ]
   -- resolve overloading, not needed in "real" programs
   let color3f = color :: Color3 GLfloat -> IO ()
   color3f (Color3 1 1 1)
   let oneIndices = [ 0, 1, 2, 3, 4, 5, 6 ] :: [GLubyte]
       twoIndices = [ 1, 7, 8, 9, 10, 11 ] :: [GLubyte]
       indices = [ oneIndices, twoIndices ]
       numIndices = genericLength indices
       count = map genericLength indices :: [GLsizei]
   withArray count $ \countBuf ->
      withMany withArray indices $ \indicesPtrs ->
         withArray indicesPtrs $ \indicesBuf ->
            multiDrawElements LineStrip countBuf UnsignedByte indicesBuf numIndices
   flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   ortho2D 0 (fromIntegral w) 0 (fromIntegral h)
   -- the following line is not in the original example, but it's good style...
   matrixMode $= Modelview 0

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _ _ _ _ = return ()

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode ]
   initialWindowSize $= Size 350 350
   initialWindowPosition $= Position 100 100
   createWindow progName
   -- we have to do this *after* createWindow, otherwise we have no OpenGL context
   exts <- get glExtensions
   unless ("GL_EXT_multi_draw_arrays" `elem` exts) $ do
      putStrLn "Sorry, this demo requires the GL_EXT_multi_draw_arrays extension."
      exitFailure
   myInit
   displayCallback $= display
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just keyboard
   mainLoop
