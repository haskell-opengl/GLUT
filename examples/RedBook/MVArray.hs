{-
   MVArray.hs (adapted from mvarray.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates multiple vertex arrays, specifically the OpenGL
   routine multiDrawElements.
-}

import Control.Monad ( unless )
import Data.List ( genericLength )
import Foreign ( Storable, Ptr, newArray )
import System.Exit ( exitFailure, exitWith, ExitCode(..) )
import Graphics.UI.GLUT

data MultiDrawInfo a = MultiDrawInfo (Ptr GLsizei) (Ptr (Ptr a)) GLsizei

makeMultiDrawInfo :: Storable a => [[a]] -> IO (MultiDrawInfo a)
makeMultiDrawInfo indicesLists = do
   count <- newArray $ map genericLength indicesLists
   indices <- newArray =<< mapM newArray indicesLists
   return $ MultiDrawInfo count indices (genericLength indicesLists)

setupPointer :: IO ()
setupPointer = do
   clientState VertexArray $= Enabled
   vertices <- newArray ([
      Vertex2  25 25,
      Vertex2  75 75,
      Vertex2 100 125,
      Vertex2 150  75,
      Vertex2 200 175,
      Vertex2 250 150,
      Vertex2 300 125,
      Vertex2 100 200,
      Vertex2 150 250,
      Vertex2 200 225,
      Vertex2 250 300,
      Vertex2 300 250 ] :: [Vertex2 GLint])
   arrayPointer VertexArray $= VertexArrayDescriptor 2 Int 0 vertices

myInit :: IO (MultiDrawInfo GLubyte)
myInit = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Smooth
   setupPointer
   makeMultiDrawInfo [ [ 0, 1, 2, 3, 4, 5, 6 ], 
                       [ 1, 7, 8, 9, 10, 11 ] ]

display :: MultiDrawInfo GLubyte -> DisplayCallback
display (MultiDrawInfo count indices primCount) = do
   clear [ ColorBuffer ]
   color (Color3 1 1 1 :: Color3 GLfloat)
   multiDrawElements LineStrip count UnsignedByte indices primCount
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
   multiDrawInfo <- myInit
   displayCallback $= display multiDrawInfo
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just keyboard
   mainLoop
