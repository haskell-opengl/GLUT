{-
   Trim.hs (adapted from trim.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program draws a NURBS surface in the shape of a symmetrical hill,
   using both a NURBS curve and pwl (piecewise linear) curve to trim part
   of the surface.

   NOTE: This example does NOT demonstrate the final NURBS API, it's currently
   just a test for the internals...
-}

import Foreign.Ptr ( castPtr )
import Foreign.Marshal ( withArray )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

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

display :: DisplayCallback
display = do
   let knots = [ 0, 0, 0, 0, 1, 1, 1, 1 ] :: [GLfloat]
       edgePt = -- counter clockwise
          [ Vertex2 0 0, Vertex2 1 0, Vertex2 1 1, Vertex2 0 1, Vertex2 0 0 ] :: [Vertex2 GLfloat]
       curvePt = -- clockwise
          [ Vertex2 0.25 0.5, Vertex2 0.25 0.75, Vertex2 0.75 0.75, Vertex2 0.75 0.5 ]  :: [Vertex2 GLfloat]
       curveKnots =
          [ 0, 0, 0, 0, 1, 1, 1, 1 ] :: [GLfloat]
       pwlPt = -- clockwise
          [Vertex2 0.75 0.5, Vertex2 0.5 0.25, Vertex2 0.25 0.5 ] :: [Vertex2 GLfloat]

   clear [ ColorBuffer, DepthBuffer ]
   preservingMatrix $ do
      rotate (330 :: GLfloat) (Vector3 1 0 0)
      scale 0.5 0.5 (0.5 :: GLfloat)

      withNURBSObj () $ \nurbsObj -> do
         setSamplingMethod nurbsObj (PathLength 25)
         setDisplayMode nurbsObj Fill'
         checkForError nurbsObj $
            nurbsBeginEndSurface nurbsObj $
               withArray (concat ctlPoints) $ \cBuf ->
                  withArray knots $ \kBuf -> do
                     gluNurbsSurface nurbsObj 8 kBuf 8 kBuf (4 * 3) 3 (castPtr cBuf) 4 4 0xdb7 -- GL_MAP2_VERTEX_3
                     nurbsBeginEndTrim nurbsObj $
                        withArray edgePt $ \edgePtBuf ->
                           gluPwlCurve nurbsObj 5 (castPtr edgePtBuf) 2 100210 -- GLU_MAP1_TRIM_2
                     nurbsBeginEndTrim nurbsObj $ do
                        withArray curveKnots $ \curveKnotsBuf ->
                           withArray curvePt $ \curvePtBuf ->
                              gluNurbsCurve nurbsObj 8 curveKnotsBuf 2 (castPtr curvePtBuf) 4 100210 -- GLU_MAP1_TRIM_2
                        withArray pwlPt $ \pwlPtBuf ->
                           gluPwlCurve nurbsObj 3 (castPtr pwlPtBuf) 2 100210 -- GLU_MAP1_TRIM_2

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

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _            _    _ _ = return ()

-- Main Loop
main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode, WithDepthBuffer ]
   initialWindowSize $= Size 500 500
   initialWindowPosition $= Position 100 100
   createWindow progName
   myInit
   reshapeCallback $= Just reshape
   displayCallback $= display
   keyboardMouseCallback $= Just keyboard
   mainLoop
