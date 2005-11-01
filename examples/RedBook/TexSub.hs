{-
   TexSub.hs  (adapted from texsub.c which is (c) Silicon Graphics, Inc)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program texture maps a checkerboard image onto two rectangles. This
   program clamps the texture, if the texture coordinates fall outside 0.0
   and 1.0. If the s key is pressed, a texture subimage is used to alter the
   original texture. If the r key is pressed, the original texture is restored.
-}

import Control.Monad ( when )
import Data.Char ( toLower )
import Data.Bits ( (.&.) )
import Foreign ( newArray )
import System.Exit ( exitFailure, exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

checkImageSize, subImageSize :: TextureSize2D
checkImageSize = TextureSize2D 64 64
subImageSize   = TextureSize2D 16 16

type Image = PixelData (Color4 GLubyte)

makeCheckImage ::
   TextureSize2D -> GLsizei -> (GLubyte -> (Color4 GLubyte)) -> IO Image
makeCheckImage (TextureSize2D w h) n f =
   fmap (PixelData RGBA UnsignedByte) $
      newArray [ f c |
                 i <- [ 0 .. w - 1 ],
                 j <- [ 0 .. h - 1 ],
                 let c | (i .&. n) == (j .&. n) = 0
                       | otherwise              = 255 ]

myInit :: IO (TextureObject, Image, Image)
myInit = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Flat
   depthFunc $= Just Less

   checkImage <- makeCheckImage checkImageSize 0x8 (\c -> Color4 c c c 255)
   subImage   <- makeCheckImage subImageSize   0x4 (\c -> Color4 c 0 0 255)
   rowAlignment Unpack $= 1

   [texName] <- genObjectNames 1
   textureBinding Texture2D $= Just texName

   textureWrapMode Texture2D S $= (Repeated, Repeat)
   textureWrapMode Texture2D T $= (Repeated, Repeat)
   textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
   texImage2D Nothing NoProxy 0  RGBA' checkImageSize 0 checkImage
   return (texName, checkImage, subImage)

display :: TextureObject -> DisplayCallback
display texName = do
   clear [ ColorBuffer, DepthBuffer ]
   texture Texture2D $= Enabled
   textureFunction $= Decal
   textureBinding Texture2D $= Just texName
   
   -- resolve overloading, not needed in "real" programs
   let texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()
       vertex3f = vertex :: Vertex3 GLfloat -> IO ()
   renderPrimitive Quads $ do
      texCoord2f (TexCoord2 0 0); vertex3f (Vertex3 (-2.0)    (-1.0)   0.0     )
      texCoord2f (TexCoord2 0 1); vertex3f (Vertex3 (-2.0)      1.0    0.0     )
      texCoord2f (TexCoord2 1 1); vertex3f (Vertex3   0.0       1.0    0.0     )
      texCoord2f (TexCoord2 1 0); vertex3f (Vertex3   0.0     (-1.0)   0.0     )

      texCoord2f (TexCoord2 0 0); vertex3f (Vertex3   1.0     (-1.0)   0.0     )
      texCoord2f (TexCoord2 0 1); vertex3f (Vertex3   1.0       1.0    0.0     )
      texCoord2f (TexCoord2 1 1); vertex3f (Vertex3   2.41421   1.0  (-1.41421))
      texCoord2f (TexCoord2 1 0); vertex3f (Vertex3   2.41421 (-1.0) (-1.41421))
   flush
   texture Texture2D $= Disabled

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   perspective 60 (fromIntegral w / fromIntegral h) 1 30
   matrixMode $= Modelview 0
   loadIdentity
   translate (Vector3 0 0 (-3.6 :: GLfloat))

keyboard :: TextureObject -> Image -> Image -> KeyboardMouseCallback
keyboard texName checkImage subImage (Char c) Down _ _ = case toLower c of
   's' -> do
      textureBinding Texture2D $= Just texName
      texSubImage2D Nothing 0 (TexturePosition2D 12 44) subImageSize subImage
      postRedisplay Nothing
   'r' -> do
      textureBinding Texture2D $= Just texName
      texImage2D Nothing NoProxy 0 RGBA' checkImageSize 0 checkImage
      postRedisplay Nothing
   '\27' -> exitWith ExitSuccess
   _ -> return ()
keyboard _ _ _ _ _ _ _ = return ()

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode, WithDepthBuffer ]
   initialWindowSize $= Size 250 250
   initialWindowPosition $= Position 100 100
   createWindow progName
   -- we have to do this *after* createWindow, otherwise we have no OpenGL context
   version <- get glVersion
   when (take 3 version == "1.0") $ do
      putStrLn "This program demonstrates a feature which is not in OpenGL Version 1.0."
      putStrLn "If your implementation of OpenGL Version 1.0 has the right extensions,"
      putStrLn "you may be able to modify this program to make it run."
      exitFailure
   (texName, checkImage, subImage) <- myInit
   displayCallback $= display texName
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just (keyboard texName checkImage subImage)
   mainLoop
