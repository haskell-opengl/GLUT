{-
   MultiTex.hs  (adapted from multitex.c which is (c) Silicon Graphics, Inc)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE
-}

import Control.Monad ( unless )
import Foreign ( withArray )
import System.Exit ( exitFailure, exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

specifyTexture :: TextureSize2D -> (GLubyte -> GLubyte -> Color4 GLubyte) -> IO ()
specifyTexture size@(TextureSize2D w h) f =
   withArray [ f i j | i <- [ 0 .. fromIntegral w - 1 ],
                       j <- [ 0 .. fromIntegral h - 1] ] $
      texImage2D Nothing NoProxy 0 RGBA' size 0 . PixelData RGBA UnsignedByte

myInit :: IO ()
myInit = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Flat
   depthFunc $= Just Less
   rowAlignment Unpack $= 1

   [texName0, texName1] <- genObjectNames 2
   textureBinding Texture2D $= Just texName0
   -- Note: We use much brighter colors than in the original example where
   -- everything was almost black.
   specifyTexture (TextureSize2D 32 32) (\i j -> Color4 (i*8) (j*8) ((i*j) `div` 4) 255)
   textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
   textureWrapMode Texture2D S $= (Repeated, Repeat)
   textureWrapMode Texture2D T $= (Repeated, Repeat)

   textureBinding Texture2D $= Just texName1
   specifyTexture (TextureSize2D 16 16) (\i j -> Color4 255 (i*16) (j*16) 255)
   textureFilter Texture2D $= ((Linear', Nothing), Linear')
   textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
   textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
   -- Use the two texture objects to define two texture units
   -- for use in multitexturing
   activeTexture $= TextureUnit 0
   texture Texture2D $= Enabled
   textureBinding Texture2D $= Just texName0
   textureFunction $= Replace
   matrixMode $= Texture
   loadIdentity
   translate (Vector3 0.5 0.5 (0 :: GLfloat))
   rotate (45 :: GLfloat) (Vector3 0 0 1)
   translate (Vector3 (-0.5) (-0.5) (0 :: GLfloat))
   matrixMode $= Modelview 0
   activeTexture $= TextureUnit 1
   texture Texture2D $= Enabled
   textureBinding Texture2D $= Just texName1
   textureFunction $= Modulate

display ::  DisplayCallback
display = do
   clear [ ColorBuffer, DepthBuffer ]
   -- resolve overloading, not needed in "real" programs
   let multiTexCoord2f = multiTexCoord :: TextureUnit -> TexCoord2 GLfloat -> IO ()
       vertex2f = vertex :: Vertex2 GLfloat -> IO ()
   renderPrimitive Triangles $ do
      multiTexCoord2f (TextureUnit 0) (TexCoord2 0   0)
      multiTexCoord2f (TextureUnit 1) (TexCoord2 1   0)
      vertex2f (Vertex2 0 0)
      multiTexCoord2f (TextureUnit 0) (TexCoord2 0.5 1)
      multiTexCoord2f (TextureUnit 1) (TexCoord2 0.5 0)
      vertex2f (Vertex2 50 100)
      multiTexCoord2f (TextureUnit 0) (TexCoord2 1   0)
      multiTexCoord2f (TextureUnit 1) (TexCoord2 1   1)
      vertex2f (Vertex2 100 0)
   flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   let wf = fromIntegral w
       hf = fromIntegral h
   if w <= h
      then ortho2D 0 100 0 (100*hf/wf)
      else ortho2D 0 (100*wf/hf) 0 100
   matrixMode $= Modelview 0
   loadIdentity

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _            _    _ _ = return ()

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode, WithDepthBuffer ]
   initialWindowSize $= Size 250 250
   initialWindowPosition $= Position 100 100
   createWindow progName
   -- we have to do this *after* createWindow, otherwise we have no OpenGL context
   exts <- get glExtensions
   unless ("GL_ARB_multitexture"   `elem` exts &&
           "GL_EXT_texture_object" `elem` exts) $ do
      putStrLn "Sorry, this demo requires the GL_ARB_multitexture and GL_EXT_texture_object extensions."
      exitFailure
   myInit
   reshapeCallback $= Just reshape
   displayCallback $= display
   keyboardMouseCallback $= Just keyboard
   mainLoop
