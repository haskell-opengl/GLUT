{-
   DOF.hs (adapted from dof.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2006 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates use of the accumulation buffer to create an
   out-of-focus depth-of-field effect. The teapots are drawn several times into
   the accumulation buffer. The viewing volume is jittered, except at the focal
   point, where the viewing volume is at the same position, each time. In this
   case, the gold teapot remains in focus.
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

myInit :: IO DisplayList
myInit = do
   ambient (Light 0) $= Color4 0 0 0 1
   diffuse (Light 0) $= Color4 1 1 1 1
   position (Light 0) $= Vertex4 0 3 3 0
    
   lightModelAmbient $= Color4 0.2 0.2 0.2 1
   lightModelLocalViewer $= Disabled

   frontFace $= CW
   lighting $= Enabled
   light (Light 0) $= Enabled
   autoNormal $= Enabled
   normalize $= Enabled
   depthFunc $= Just Less

   clearColor $= Color4 0 0 0 0
   clearAccum $= Color4 0 0 0 0
   -- make teapot display list
   defineNewList Compile $
      renderObject Solid (Teapot 0.5)

-- Move object into position, specify the material properties, draw a teapot.
renderTeapot :: DisplayList -> Vector3 GLfloat -> Color4 GLfloat
             -> Color4 GLfloat -> Color4 GLfloat -> GLfloat -> IO ()
renderTeapot teapotList pos amb dif spec shine =
   preservingMatrix $ do
      translate pos
      materialAmbient Front $= amb
      materialDiffuse Front $= dif
      materialSpecular Front $= spec
      materialShininess Front $= shine * 128
      callList teapotList

-- display draws 5 teapots into the accumulation buffer several times; each time
-- with a jittered perspective. The focal point is at z = 5.0, so the gold
-- teapot will stay in focus. The amount of jitter is adjusted by the magnitude
-- of the accPerspective jitter; in this example, 0.33. In this example, the
-- teapots are drawn 8 times.
display :: DisplayList -> DisplayCallback
display teapotList = do
   (_, Size w h) <- get viewport
   clear [ AccumBuffer ]

   flip mapM_ j8 $ \(Vector2 x y) -> do
      clear [ ColorBuffer, DepthBuffer ]
      accPerspective 45 (fromIntegral w / fromIntegral h) 1 15
                     (Vector2 0 0) (Vector2 (0.33 * x) (0.33 * y)) 5

      -- ruby, gold, silver, emerald, and cyan teapots
      renderTeapot teapotList
                   (Vector3 (-1.1) (-0.5) (-4.5))
                   (Color4 0.1745     0.01175    0.01175    1)
                   (Color4 0.61424    0.04136    0.04136    1)
                   (Color4 0.727811   0.626959   0.626959   1)
                   0.6
      renderTeapot teapotList
                   (Vector3 (-0.5) (-0.5) (-5.0))
                   (Color4 0.24725    0.1995     0.0745     1)
                   (Color4 0.75164    0.60648    0.22648    1)
                   (Color4 0.628281   0.555802   0.366065   1)
                   0.4
      renderTeapot teapotList
                   (Vector3   0.2  (-0.5) (-5.5))
                   (Color4 0.19225    0.19225    0.19225    1)
                   (Color4 0.50754    0.50754    0.50754    1)
                   (Color4 0.508273   0.508273   0.508273   1)
                   0.4
      renderTeapot teapotList
                   (Vector3   1.0  (-0.5) (-6.0))
                   (Color4 0.0215     0.1745     0.0215     1)
                   (Color4 0.07568    0.61424    0.07568    1)
                   (Color4 0.633      0.727811   0.633      1)
                   0.6
      renderTeapot teapotList
                   (Vector3   1.8  (-0.5) (-6.5))
                   (Color4 0.0        0.1        0.06       1)
                   (Color4 0.0        0.50980392 0.50980392 1)
                   (Color4 0.50196078 0.50196078 0.50196078 1)
                    0.25
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
   initialWindowSize $= Size 400 400
   initialWindowPosition $= Position 100 100
   createWindow progName
   teapotList <- myInit
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just keyboard
   displayCallback $= display teapotList
   mainLoop
