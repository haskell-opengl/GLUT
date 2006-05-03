{-
   AccPersp.hs (adapted from accpersp.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2006 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   Use the accumulation buffer to do full-scene antialiasing on a scene with
   perspective projection, using the special routines accFrustum and
   accPerspective.
-}

import Data.List ( genericLength )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

-- j8 contains values in the range -.5 < x < .5, -.5 < y < .5, and have a
-- gaussian distribution around the origin. Use these to do model jittering for
-- scene anti-aliasing and view volume jittering for depth of field effects. Use
-- in conjunction with the accwindow routine.
j8 :: [Vector2 GLdouble]
j8 = [
   Vector2 (-0.334818)   0.435331 ,
   Vector2   0.286438  (-0.393495),
   Vector2   0.459462    0.141540 ,
   Vector2 (-0.414498) (-0.192829),
   Vector2 (-0.183790)   0.082102 ,
   Vector2 (-0.079263) (-0.317383),
   Vector2   0.102254    0.299133 ,
   Vector2   0.164216  (-0.054399) ]

-- The first 6 arguments are identical to the frustum call. pixD is anti-alias
-- jitter in pixels. Use (Vector2 0 0) for no anti-alias jitter. eyeD is
-- depth-of field jitter in pixels. Use (Vector2 0 0) for no depth of field
-- effects. focus is distance from eye to plane in focus. focus must be greater
-- than, but not equal to 0. Note that accFrustum calls translate. You will
-- probably want to insure that your ModelView matrix has been initialized to
-- identity before calling accFrustum.
accFrustum :: GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble
           -> Vector2 GLdouble -> Vector2 GLdouble -> GLdouble -> IO ()
accFrustum left right bottom top zNear zFar
           (Vector2 pixDx pixDy) (Vector2 eyeDx eyeDy) focus = do
   (_, Size w h) <- get viewport
	
   let xWSize = right - left;
       yWSize = top - bottom;
	
       dx = -(pixDx * xWSize / fromIntegral w + eyeDx * zNear / focus)
       dy = -(pixDy * yWSize / fromIntegral h + eyeDy * zNear / focus)
	
   matrixMode $= Projection
   loadIdentity
   frustum (left + dx) (right + dx) (bottom + dy) (top + dy) zNear zFar
   matrixMode $= Modelview 0
   loadIdentity
   translate (Vector3 (-eyeDx) (-eyeDy) 0)

-- The first 4 arguments are identical to the perspective call. pixD is
-- anti-alias jitter in pixels. Use (Vector2 0 0) for no anti-alias jitter. eyeD
-- is depth-of field jitter in pixels. Use (Vector2 0 0) for no depth of field
-- effects. focus is distance from eye to plane in focus. focus must be greater
-- than, but not equal to 0. Note that accPerspective calls accFrustum.
accPerspective :: GLdouble -> GLdouble -> GLdouble -> GLdouble
               -> Vector2 GLdouble -> Vector2 GLdouble -> GLdouble -> IO ()
accPerspective fovY aspect zNear zFar  pixD eyeD focus = do
   let fov2 = ((fovY * pi) / 180) / 2

       top = zNear / (cos fov2 / sin fov2)
       bottom = -top

       right = top * aspect
       left = -right

   accFrustum left right bottom top zNear zFar pixD eyeD focus

-- Initialize lighting and other values.
myInit :: IO ()
myInit = do
   materialAmbient Front $= Color4 1 1 1 1
   materialSpecular Front $= Color4 1 1 1 1
   materialShininess Front $= 50
   position (Light 0) $= Vertex4 0 0 10 1
   lightModelAmbient $= Color4 0.2 0.2 0.2 1

   lighting $= Enabled
   light (Light 0) $= Enabled
   depthFunc $= Just Less
   shadeModel $= Flat

   clearColor $= Color4 0 0 0 0
   clearAccum $= Color4 0 0 0 0

displayObjects :: IO ()
displayObjects = do
   -- resolve overloading, not needed in "real" programs
   let translatef = translate :: Vector3 GLfloat -> IO ()
       rotatef = rotate :: GLfloat -> Vector3 GLfloat -> IO ()

   preservingMatrix $ do
      translatef (Vector3 0 0 (-5))
      rotatef 30 (Vector3 1 0 0)

      preservingMatrix $ do
         translatef (Vector3 (-0.80) 0.35 0)
         rotatef 100 (Vector3 1 0 0)
         materialDiffuse Front $= Color4 0.7 0.7 0 1
         renderObject Solid (Torus 0.275 0.85 16 16)

      preservingMatrix $ do
         translatef (Vector3 (-0.75) (-0.50) 0)
         rotatef 45 (Vector3 0 0 1)
         rotatef 45 (Vector3 1 0 0)
         materialDiffuse Front $= Color4 0 0.7 0.7 1
         renderObject Solid (Cube 1.5)

      preservingMatrix $ do
         translatef (Vector3 0.75 0.60 0)
         rotatef 30 (Vector3 1 0 0)
         materialDiffuse Front $= Color4 0.7 0 0.7 1
         renderObject Solid (Sphere' 1 16 16)

      preservingMatrix $ do
         translatef (Vector3 0.70 (-0.90) 0.25)
         materialDiffuse Front $= Color4 0.7 0.4 0.4 1
         renderObject Solid Octahedron

-- display draws 5 teapots into the accumulation buffer several times; each time
-- with a jittered perspective. The focal point is at z = 5.0, so the gold
-- teapot will stay in focus. The amount of jitter is adjusted by the magnitude
-- of the accPerspective jitter; in this example, 0.33. In this example, the
-- teapots are drawn 8 times.
display :: DisplayCallback
display = do
   (_, Size w h) <- get viewport
   clear [ AccumBuffer ]

   flip mapM_ j8 $ \(Vector2 x y) -> do
      clear [ ColorBuffer, DepthBuffer ]
      accPerspective 50 (fromIntegral w / fromIntegral h) 1 15
                     (Vector2 x y) (Vector2 0 0) 1
      displayObjects
      accum Accum (recip (genericLength j8))

   accum Return 1
   flush

reshape :: ReshapeCallback
reshape size = do
   viewport $= (Position 0 0, size)

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _            _    _ _ = return ()

-- Main Loop: Be certain you request an accumulation buffer.
main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode, WithAccumBuffer, WithDepthBuffer ]
   initialWindowSize $= Size 250 250
   initialWindowPosition $= Position 100 100
   createWindow progName
   myInit
   reshapeCallback $= Just reshape
   displayCallback $= display
   keyboardMouseCallback $= Just keyboard
   mainLoop
