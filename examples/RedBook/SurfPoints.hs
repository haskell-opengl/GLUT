{-
   SurfPoints.hs (adapted from surfpoints.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program is a modification of the earlier Surface.hs program. The
   vertex data are not directly rendered, but are instead passed to the
   callback function. The values of the tessellated vertices are printed
   out there.

   This program draws a NURBS surface in the shape of a symmetrical hill.
   The 'c' keyboard key allows you to toggle the visibility of the control
   points themselves. Note that some of the control points are hidden by
   the surface itself.

   NOTE: This example does NOT demonstrate the final NURBS API, it's currently
   just a test for the internals...
-}

import Control.Monad ( when, unless )
import Data.Char ( toLower )
import Data.IORef ( IORef, newIORef )
import Foreign.Ptr ( castPtr )
import Foreign.Marshal ( withArray )
import System.Exit ( exitWith, ExitCode(ExitSuccess), exitFailure )
import Graphics.UI.GLUT

data State = State { showPoints :: IORef Bool }

makeState :: IO State
makeState = do
   s <- newIORef False
   return $ State { showPoints = s }

-- The control points of the surface form a small hill and
-- range from -3 to +3 in x, y, and z.
ctlPoints :: [[Vertex3 GLfloat]]
ctlPoints =
   [ [ Vertex3 (2 * u - 3)
               (2 * v - 3)
               (if (u == 1 || u ==2) && (v == 1 || v == 2) then 3 else -3)
     | v <- [ 0 .. 3 ] ]
   | u <- [ 0 .. 3 ]]

myInit :: IO ()
myInit = do
   clearColor $= Color4 0 0 0 0
   materialDiffuse Front $= Color4 0.7 0.7 0.7 1
   materialSpecular Front $= Color4 1 1 1 1
   materialShininess Front $= 100

   lighting $= Enabled
   light (Light 0) $= Enabled
   depthFunc $= Just Less
   autoNormal $= Enabled
   normalize $= Enabled

--------------------------------------------------------------------------------

display :: State -> DisplayCallback
display state = do
   let knots = [ 0, 0, 0, 0, 1, 1, 1, 1 ] :: [GLfloat]
   clear [ ColorBuffer, DepthBuffer ]
   preservingMatrix $ do
      rotate (330 :: GLfloat) (Vector3 1 0 0)
      scale 0.5 0.5 (0.5 :: GLfloat)

      withNURBSObj () $ \nurbsObj -> do
         setNURBSMode nurbsObj NURBSTessellator
         setSamplingMethod nurbsObj (PathLength 25)
         setDisplayMode nurbsObj Fill'
         checkForError nurbsObj $
            withBeginCallback nurbsObj print $
               withVertexCallback nurbsObj print $
                  withNormalCallback nurbsObj print $
                     withEndCallback nurbsObj (putStrLn "end") $
                        nurbsBeginEndSurface nurbsObj $
                           withArray (concat ctlPoints) $ \cBuf ->
                              withArray knots $ \kBuf ->
                                 gluNurbsSurface nurbsObj 8 kBuf 8 kBuf (4 * 3) 3 (castPtr cBuf) 4 4 0xdb7 -- GL_MAP2_VERTEX_3

      s <- get (showPoints state)
      when s $ do
         pointSize $= 5
         lighting $= Disabled
         color (Color3 1 1 (0 :: GLfloat))
         renderPrimitive Points $
            mapM_ (mapM_ vertex) ctlPoints
         lighting $= Enabled

   flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   perspective 45 (fromIntegral w / fromIntegral h) 3 8
   matrixMode $= Modelview 0
   loadIdentity
   translate (Vector3 0 0 (-5 :: GLfloat))

keyboard :: State -> KeyboardMouseCallback
keyboard state (Char c) Down _ _ = case toLower c of
   'c'   -> do showPoints state $~ not; postRedisplay Nothing
   '\27' -> exitWith ExitSuccess
   _     -> return ()
keyboard _ _ _ _ _ = return ()

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode, WithDepthBuffer ]
   initialWindowSize $= Size 500 500
   initialWindowPosition $= Position 100 100
   createWindow progName
   version <- get gluVersion
   unless (take 3 version == "1.3") $ do
      putStrLn "This program demonstrates a feature which is introduced in the"
      putStrLn "OpenGL Utility Library (GLU) Version 1.3."
      putStrLn "If your implementation of GLU has the right extensions,"
      putStrLn "you may be able to modify this program to make it run."
      exitFailure
   state <- makeState
   myInit
   reshapeCallback $= Just reshape
   displayCallback $= display state
   keyboardMouseCallback $= Just (keyboard state)
   mainLoop
