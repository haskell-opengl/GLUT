{-
   PointP.hs (adapted from pointp.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2006 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates point parameters and their effect on point
   primitives. 250 points are randomly generated within a 10 by 10 by 40 region,
   centered at the origin. In some modes (including the default), points that
   are closer to the viewer will appear larger.

   Pressing the 'c', 'l', and 'q' keys switch the point parameters attenuation
   mode to constant, linear, or quadratic, respectively.

   Pressing the 'f' and 'b' keys move the viewer forward and backwards. In
   either linear or quadratic attenuation mode, the distance from the viewer to
   the point will change the size of the point primitive.

   Pressing the '+' and '-' keys will change the current point size. In this
   program, the point size is bounded, so it will not get less than 2, nor
   greater than the maximum returned by pointSizeRange.
-}

import Control.Monad ( when, unless )
import Data.IORef ( IORef, newIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess), exitFailure )
import System.Random ( randomRIO )
import Graphics.UI.GLUT

type Attenuation = (GLfloat,GLfloat,GLfloat)

constant, linear, quadratic :: Attenuation
constant  = (1, 0,    0   )
linear    = (0, 0.12, 0   )
quadratic = (0, 0,    0.01)

data State = State { distance :: IORef GLfloat }

makeState :: IO State
makeState = do
   d <- newIORef (-10)
   return $ State { distance = d }

randomColor :: IO (Color3 GLfloat)
randomColor = do
   g <- randomRIO (0.5, 1)
   b <- randomRIO (0, 1)
   return $ Color3 1 g b

randomVertex :: IO (Vertex3 GLfloat)
randomVertex = do
   x <- randomRIO (-5, 5)
   y <- randomRIO (-5, 5)
   z <- randomRIO (-5, -45)
   return $ Vertex3 x y z

myInit :: IO DisplayList
myInit = do
   pointList <- defineNewList Compile $
      renderPrimitive Points $
         sequence_ $ replicate 250 $ do
            color =<< randomColor
            vertex =<< randomVertex

   depthFunc $= Just Less
   pointSmooth $= Enabled
   blend $= Enabled
   blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
   pointSize $= 7

   pointDistanceAttenuation $= linear
   pointFadeThresholdSize $= 2

   return pointList

display :: State -> DisplayList -> DisplayCallback
display state pointList = do
   clear [ ColorBuffer, DepthBuffer ]
   d <- get (distance state)
   loadIdentity
   translate (Vector3 0 0 d)
   callList pointList
   swapBuffers

reshape :: ReshapeCallback
reshape size = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   perspective  35 1 0.25 200
   matrixMode $= Modelview 0

setPointDistanceAttenuation :: Attenuation -> IO ()
setPointDistanceAttenuation att = do
   pointDistanceAttenuation $= att
   postRedisplay Nothing

incDistance :: State -> GLfloat -> IO ()
incDistance state inc = do
   distance state $~ (+ inc)
   postRedisplay Nothing

incPointSize :: GLfloat -> IO ()
incPointSize inc = do
   newPointSize <- fmap (+ inc) $ get pointSize
   (_,maxPointSize) <- get pointSizeRange
   when (2 <= newPointSize && newPointSize <= maxPointSize) $ do
      pointSize $= newPointSize
      postRedisplay Nothing

keyboard :: State -> KeyboardMouseCallback
keyboard state (Char c) Down _ _ = case c of
   'c'   -> setPointDistanceAttenuation constant
   'l'   -> setPointDistanceAttenuation linear
   'q'   -> setPointDistanceAttenuation quadratic
   'b'   -> incDistance state (-0.5)
   'f'   -> incDistance state 0.5
   '+'   -> incPointSize 1
   '-'   -> incPointSize (-1)
   '\27' -> exitWith ExitSuccess
   _     -> return ()
keyboard _ _ _ _ _ = return ()

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ RGBMode, DoubleBuffered, WithDepthBuffer, Multisampling ]
   initialWindowSize $= Size 500 500
   initialWindowPosition $= Position 100 100
   createWindow progName

   -- We have to do this *after* createWindow, otherwise we have no OpenGL
   -- context. Note that the original C example simply tests for OpenGL 1.4 at
   -- compile time, we do a runtime check for the needed extension.
   extensions <- get glExtensions
   unless ("GL_ARB_point_parameters" `elem` extensions) $ do
      putStrLn "Sorry, this demo requires the GL_ARB_point_parameters extension."
      exitFailure

   state <- makeState
   pointList <- myInit
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just (keyboard state)
   displayCallback $= display state pointList
   mainLoop
