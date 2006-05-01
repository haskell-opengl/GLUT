{-
   Torus.hs (adapted from torus.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2006 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates the creation of a display list.
-}

import Data.Char ( toLower )
import Data.IORef ( IORef, newIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

data State = State { spinX, spinY :: IORef GLfloat }

makeState :: IO State
makeState = do
   x <- newIORef 0
   y <- newIORef 0
   return $ State { spinX = x, spinY = y }

torus :: Int -> Int -> IO ()
torus numC numT = do
   let stepC = 2 * pi / fromIntegral numC :: GLfloat
       stepT = 2 * pi / fromIntegral numT
   flip mapM_ [ 0 .. numC - 1 ] $ \i ->
      renderPrimitive QuadStrip $
         flip mapM_ [ 0 .. numT ] $ \j ->
            flip mapM_ [ 1, 0 ] $ \k -> do
               let s = (fromIntegral ((i + k) `mod` numC) + 0.5) * stepC
                   t = (fromIntegral (      j `mod` numT)      ) * stepT
                   x = (1 + 0.1 * cos s) * cos t
                   y = (1 + 0.1 * cos s) * sin t
                   z =      0.1 * sin s
               vertex (Vertex3 x y z)

myInit :: IO DisplayList
myInit = do
   theTorus <- defineNewList Compile $
      torus 8 25
   shadeModel $= Flat
   clearColor $= Color4 0 0 0 0
   return theTorus

display :: State -> DisplayList -> DisplayCallback
display state theTorus = do
   clear [ ColorBuffer ]
   loadIdentity
   lookAt (Vertex3 0 0 10) (Vertex3 0 0 0) (Vector3 0 1 0)
   x <- get (spinX state)
   rotate x (Vector3 1 0 0)
   y <- get (spinY state)
   rotate y (Vector3 0 1 0)
   color (Color3 1 1 (1 :: GLfloat))
   callList theTorus
   flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   perspective  30 (fromIntegral w / fromIntegral h) 1 100
   matrixMode $= Modelview 0

incSpin :: IORef GLfloat -> IO ()
incSpin spinRef = do
   let wrap n s = if s > n then s - n else s
   spinRef $~ (wrap 360 . (+ 30))
   postRedisplay Nothing

keyboard :: State -> KeyboardMouseCallback
keyboard state (Char c) Down _ _ = case toLower c of
   'x'   -> incSpin (spinX state)
   'y'   -> incSpin (spinY state)
   'i'   -> do spinX state $= 0; spinY state $= 0; postRedisplay Nothing
   '\27' -> exitWith ExitSuccess
   _     -> return ()
keyboard _ _ _ _ _ = return ()

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode ]
   initialWindowSize $= Size 200 200
   createWindow progName
   state <- makeState
   theTorus <- myInit
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just (keyboard state)
   displayCallback $= display state theTorus
   mainLoop
