{-
   Alpha.hs (adapted from alpha.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program draws several overlapping filled polygons to demonstrate the
   effect order has on alpha blending results. Use the 't' key to toggle the
   order of drawing polygons.
-}

import Data.Char ( toLower )
import Data.IORef ( IORef, newIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

data State = State { leftFirst :: IORef Bool }

makeState :: IO State
makeState = do
   l <- newIORef True
   return $ State { leftFirst = l }

-- Initialize alpha blending function.
myInit :: IO ()
myInit = do
   blend $= Enabled
   blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
   shadeModel $= Flat
   clearColor $= Color4 0 0 0 0

drawLeftTriangle :: IO ()
drawLeftTriangle =
   -- draw yellow triangle on LHS of screen
   renderPrimitive Triangles $ do
      color (Color4 1 1 0 (0.75 :: GLfloat))
      vertex (Vertex3 0.1 0.9 (0 :: GLfloat))
      vertex (Vertex3 0.1 0.1 (0 :: GLfloat))
      vertex (Vertex3 0.7 0.5 (0 :: GLfloat))

drawRightTriangle :: IO ()
drawRightTriangle =
   -- draw cyan triangle on RHS of screen
   renderPrimitive Triangles $ do
      color (Color4 0 1 1 (0.75 :: GLfloat))
      vertex (Vertex3 0.9 0.9 (0 :: GLfloat))
      vertex (Vertex3 0.3 0.5 (0 :: GLfloat))
      vertex (Vertex3 0.9 0.1 (0 :: GLfloat))

display :: State -> DisplayCallback
display state = do
   clear [ ColorBuffer ]
   l <- get (leftFirst state)
   if l
      then do drawLeftTriangle; drawRightTriangle
      else do drawRightTriangle; drawLeftTriangle
   flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   let wf = fromIntegral w
       hf = fromIntegral h
   if w <= h
      then ortho2D 0 1 0 (hf/wf)
      else ortho2D 0 (wf/hf) 0 1

keyboard :: State -> KeyboardMouseCallback
keyboard state (Char c) Down _ _ = case toLower c of
   't'   -> do leftFirst state $~ not; postRedisplay Nothing
   '\27' -> exitWith ExitSuccess   -- Escape key
   _     -> return ()
keyboard _ _ _ _ _ = return ()

-- Main Loop
-- Open window with initial window size, title bar, RGBA display mode, and
-- handle input events.
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
