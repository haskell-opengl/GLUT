{-
   Teapots.hs (adapted from teapots.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates lots of material properties. A single light
   source illuminates the objects.
-}

import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

-- Initialize depth buffer, projection matrix, light source, and lighting
-- model. Do not specify a material property here.
myInit :: IO DisplayList
myInit = do
   ambient (Light 0) $= Color4 0 0 0 1
   diffuse (Light 0) $= Color4 1 1 1 1
   position (Light 0) $= Vertex4  0 3 3 0
   lightModelAmbient $= Color4 0.2 0.2 0.2 1
   lightModelLocalViewer $= Disabled

   frontFace $= CW
   lighting $= Enabled
   light (Light 0) $= Enabled
   autoNormal $= Enabled
   normalize $= Enabled
   depthFunc $= Just Less
   -- be efficient--make teapot display list
   defineNewList Compile $
      renderObject Solid (Teapot 1)

-- Move object into position, specify the material properties, draw a teapot.
renderTeapot :: DisplayList -> Vector3 GLfloat -> Color4 GLfloat
             -> Color4 GLfloat -> Color4 GLfloat -> GLfloat -> IO ()
renderTeapot teapotList pos amb dif spec shine = do
   preservingMatrix $ do
      translate pos
      materialAmbient Front $= amb
      materialDiffuse Front $= dif
      materialSpecular Front $= spec
      materialShininess Front $= shine * 128
      callList teapotList

-- 1st column: emerald, jade, obsidian, pearl, ruby, turquoise
-- 2nd column: brass, bronze, chrome, copper, gold, silver
-- 3rd column: black, cyan, green, red, white, yellow plastic
-- 4th column: black, cyan, green, red, white, yellow rubber
display :: DisplayList -> DisplayCallback
display teapotList = do
   clear [ ColorBuffer, DepthBuffer ]
   renderTeapot teapotList
                (Vector3  2 17 0)
                (Color4 0.0215    0.1745      0.0215     1)
                (Color4 0.07568   0.61424     0.07568    1)
                (Color4 0.633     0.727811    0.633      1)
                0.6
   renderTeapot teapotList
                (Vector3  2 14 0)
                (Color4 0.135     0.2225      0.1575     1)
                (Color4 0.54      0.89        0.63       1)
                (Color4 0.316228  0.316228    0.316228   1)
                0.1
   renderTeapot teapotList
                (Vector3  2 11 0)
                (Color4 0.05375   0.05        0.06625    1)
                (Color4 0.18275   0.17        0.22525    1)
                (Color4 0.332741  0.328634    0.346435   1)
                0.3
   renderTeapot teapotList
                (Vector3  2  8 0)
                (Color4 0.25      0.20725     0.20725    1)
                (Color4 1         0.829       0.829      1)
                (Color4 0.296648  0.296648    0.296648   1)
                0.088
   renderTeapot teapotList
                (Vector3  2  5 0)
                (Color4 0.1745    0.01175     0.01175    1)
                (Color4 0.61424   0.04136     0.04136    1)
                (Color4 0.727811  0.626959    0.626959   1)
                0.6
   renderTeapot teapotList
                (Vector3  2  2 0)
                (Color4 0.1       0.18725     0.1745     1)
                (Color4 0.396     0.74151     0.69102    1)
                (Color4 0.297254  0.30829     0.306678   1)
                0.1
   renderTeapot teapotList
                (Vector3  6 17 0)
                (Color4 0.329412  0.223529    0.027451   1)
                (Color4 0.780392  0.568627    0.113725   1)
                (Color4 0.992157  0.941176    0.807843   1)
                0.21794872
   renderTeapot teapotList
                (Vector3  6 14 0)
                (Color4 0.2125    0.1275      0.054      1)
                (Color4 0.714     0.4284      0.18144    1)
                (Color4 0.393548  0.271906    0.166721   1)
                0.2
   renderTeapot teapotList
                (Vector3  6 11 0)
                (Color4 0.25      0.25        0.25       1)
                (Color4 0.4       0.4         0.4        1)
                (Color4 0.774597  0.774597    0.774597   1)
                0.6
   renderTeapot teapotList
                (Vector3  6  8 0)
                (Color4 0.19125   0.0735      0.0225     1)
                (Color4 0.7038    0.27048     0.0828     1)
                (Color4 0.256777  0.137622    0.086014   1)
                0.1
   renderTeapot teapotList
                (Vector3  6  5 0)
                (Color4 0.24725   0.1995      0.0745     1)
                (Color4 0.75164   0.60648     0.22648    1)
                (Color4 0.628281  0.555802    0.366065   1)
                0.4
   renderTeapot teapotList
                (Vector3  6  2 0)
                (Color4 0.19225   0.19225     0.19225    1)
                (Color4 0.50754   0.50754     0.50754    1)
                (Color4 0.508273  0.508273    0.508273   1)
                0.4
   renderTeapot teapotList
                (Vector3 10 17 0)
                (Color4 0         0           0          1)
                (Color4 0.01      0.01        0.01       1)
                (Color4 0.50      0.50        0.50       1)
                0.25
   renderTeapot teapotList
                (Vector3 10 14 0)
                (Color4 0          0.1        0.06       1)
                (Color4 0          0.50980392 0.50980392 1)
                (Color4 0.50196078 0.50196078 0.50196078 1)
                0.25
   renderTeapot teapotList
                (Vector3 10 11 0)
                (Color4 0         0           0          1)
                (Color4 0.1       0.35        0.1        1)
                (Color4 0.45      0.55        0.45       1)
                0.25
   renderTeapot teapotList
                (Vector3 10  8 0)
                (Color4 0         0           0          1)
                (Color4 0.5       0           0          1)
                (Color4 0.7       0.6         0.6        1)
                0.25
   renderTeapot teapotList
                (Vector3 10  5 0)
                (Color4 0         0           0          1)
                (Color4 0.55      0.55        0.55       1)
                (Color4 0.70      0.70        0.70       1)
                0.25
   renderTeapot teapotList
                (Vector3 10  2 0)
                (Color4 0         0           0          1)
                (Color4 0.5       0.5         0          1)
                (Color4 0.60      0.60        0.50       1)
                0.25
   renderTeapot teapotList
                (Vector3 14 17 0)
                (Color4 0.02      0.02        0.02       1)
                (Color4 0.01      0.01        0.01       1)
                (Color4 0.4       0.4         0.4        1)
                0.078125
   renderTeapot teapotList
                (Vector3 14 14 0)
                (Color4 0         0.05        0.05       1)
                (Color4 0.4       0.5         0.5        1)
                (Color4 0.04      0.7         0.7        1)
                0.078125
   renderTeapot teapotList
                (Vector3 14 11 0)
                (Color4 0         0.05        0          1)
                (Color4 0.4       0.5         0.4        1)
                (Color4 0.04      0.7         0.04       1)
                0.078125
   renderTeapot teapotList
                (Vector3 14  8 0)
                (Color4 0.05      0           0          1)
                (Color4 0.5       0.4         0.4        1)
                (Color4 0.7       0.04        0.04       1)
                0.078125
   renderTeapot teapotList
                (Vector3 14  5 0)
                (Color4 0.05      0.05        0.05       1)
                (Color4 0.5       0.5         0.5        1)
                (Color4 0.7       0.7         0.7        1)
                0.078125
   renderTeapot teapotList
                (Vector3 14  2 0)
                (Color4 0.05      0.05        0          1)
                (Color4 0.5       0.5         0.4        1)
                (Color4 0.7       0.7         0.04       1)
                0.078125
   flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   let wf = fromIntegral w
       hf = fromIntegral h
   if w <= h
      then ortho 0 16 0 (16 * hf/wf) (-10) 10
      else ortho 0 (16 * wf/hf) 0 16 (-10) 10
   matrixMode $= Modelview 0

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _            _    _ _ = return ()

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode, WithDepthBuffer ]
   initialWindowSize $= Size 500 600
   initialWindowPosition $= Position 50 50
   createWindow progName
   teapotList <- myInit
   reshapeCallback $= Just reshape
   displayCallback $= display teapotList
   keyboardMouseCallback $= Just keyboard
   mainLoop
