{-
   CubeMap.hs  (adapted from CubeMap.c which is (c) Silicon Graphics, Inc)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates cube map textures. Six different colored checker
   board textures are created and applied to a lit sphere.

   Pressing the 'f' and 'b' keys translate the viewer forward and backward.
-}

import Data.Bits ( (.&.) )
import Data.IORef ( IORef, newIORef )
import Foreign ( withArray )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

data State = State { zTrans :: IORef GLfloat }

makeState :: IO State
makeState = do
   z <- newIORef 0
   return $ State { zTrans = z }

imageSize :: TextureSize2D
imageSize = TextureSize2D 4 4

withCheckImage :: TextureSize2D -> GLsizei -> (GLubyte -> (Color4 GLubyte))
               -> (PixelData (Color4 GLubyte) -> IO ()) -> IO ()
withCheckImage (TextureSize2D w h) n f act =
   withArray [ f c |
               i <- [ 0 .. w - 1 ],
               j <- [ 0 .. h - 1 ],
               let c | (i .&. n) == (j .&. n) = 0
                     | otherwise              = 255 ] $
   act. PixelData RGBA UnsignedByte

makeImage :: CubeMapTarget -> (GLubyte -> (Color4 GLubyte)) -> IO ()
makeImage target f =
   withCheckImage imageSize 0x1 f $
      texImage2D (Just target) NoProxy 0  RGBA' imageSize 0

myInit :: IO ()
myInit = do
   clearColor $= Color4 0 0 0 0
   depthFunc $= Just Less
   shadeModel $= Smooth

   rowAlignment Unpack $= 1
   textureWrapMode TextureCubeMap S $= (Repeated, Repeat)
   textureWrapMode TextureCubeMap T $= (Repeated, Repeat)
   textureWrapMode TextureCubeMap R $= (Repeated, Repeat)
   textureFilter TextureCubeMap $= ((Nearest, Nothing), Nearest)

   makeImage TextureCubeMapPositiveX (\c -> Color4   c c   c 255)
   makeImage TextureCubeMapNegativeX (\c -> Color4   0 c   c 255)
   makeImage TextureCubeMapPositiveY (\c -> Color4   c c   0 255)
   makeImage TextureCubeMapNegativeY (\c -> Color4 255 c   c 255)
   makeImage TextureCubeMapPositiveZ (\c -> Color4   c 0   c 255)
   makeImage TextureCubeMapNegativeZ (\c -> Color4   c c 255 255)

   textureGenMode S $= Just NormalMap
   textureGenMode T $= Just NormalMap
   textureGenMode R $= Just NormalMap

   textureFunction $= Modulate

   texture TextureCubeMap $= Enabled
   lighting $= Enabled
   light (Light 0) $= Enabled
   autoNormal $= Enabled
   normalize $= Enabled
   materialDiffuse Front $= Color4 1 1 1 1

display :: State -> DisplayCallback
display state = do
   clear [ ColorBuffer, DepthBuffer ]
   preservingMatrix $ do
      z <- get (zTrans state)
      translate (Vector3 0 0 z)
      renderObject Solid (Sphere' 5 20 10)
   swapBuffers

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   perspective 40 (fromIntegral w / fromIntegral h) 1 300
   matrixMode $= Modelview 0
   loadIdentity
   translate (Vector3 0 0 (-20 :: GLfloat))

keyboard :: State -> KeyboardMouseCallback
keyboard state (Char 'f')   Down _ _ = move state (-0.2)
keyboard state (Char 'b')   Down _ _ = move state   0.2
keyboard _     (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _     _            _    _ _ = return ()

move :: State -> GLfloat -> IO ()
move state inc = do
   zTrans state $~ (+ inc)
   postRedisplay Nothing

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ DoubleBuffered, RGBMode, WithDepthBuffer ]
   initialWindowSize $= Size 400 400
   initialWindowPosition $= Position 50 50
   createWindow progName
   state <- makeState
   myInit
   displayCallback $= display state
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just (keyboard state)
   mainLoop
