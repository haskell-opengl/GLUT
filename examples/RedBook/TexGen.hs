{-
   TexGen.hs  (adapted from texgen.c which is (c) Silicon Graphics, Inc)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program draws a texture mapped teapot with automatically generated
   texture coordinates. The texture is rendered as stripes on the teapot.
   Initially, the object is drawn with texture coordinates based upon the
   object coordinates of the vertex and distance from the plane x = 0.
   Pressing the 'e' key changes the coordinate generation to eye coordinates
   of the vertex. Pressing the 'o' key switches it back to the object
   coordinates. Pressing the 's' key changes the plane to a slanted one
   (x + y + z = 0). Pressing the 'x' key switches it back to x = 0.
-}

import Control.Monad ( when )
import Data.Char ( toLower )
import Data.Maybe ( isJust, listToMaybe )
import Foreign ( withArray )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

stripeImageWidth :: TextureSize1D
stripeImageWidth = TextureSize1D 32

xEqualZero, slanted :: Plane GLdouble
xEqualZero = Plane 1 0 0 0
slanted    = Plane 1 1 1 0

withStripeImage :: (PixelData (Color4 GLubyte) -> IO a) -> IO a
withStripeImage act =
   withArray [ Color4 (if j <= 4 then 255 else 0)
                      (if j >  4 then 255 else 0)
                      0
                      255
             | j <- [ 0 .. w - 1 ] ] $
      act . PixelData RGBA UnsignedByte
   where TextureSize1D w = stripeImageWidth

myInit :: IO (Maybe TextureObject)
myInit = do
   clearColor $= Color4 0 0 0 0
   depthFunc $= Just Less
   shadeModel $= Smooth
   rowAlignment Unpack $= 1

   exts <- get glExtensions
   mbTexName <- if "GL_EXT_texture_object" `elem` exts
                   then fmap listToMaybe $ genObjectNames 1
                   else return Nothing
   when (isJust mbTexName) $ textureBinding Texture1D $= mbTexName

   textureWrapMode Texture1D S $= (Repeated, Repeat)
   textureFilter Texture1D $= ((Linear', Nothing), Linear')
   withStripeImage $ texImage1D NoProxy 0  RGBA' stripeImageWidth 0

   textureFunction $= Modulate
   textureGenMode S $= Just (ObjectLinear xEqualZero)

   texture Texture1D $= Enabled
   lighting $= Enabled
   light (Light 0) $= Enabled
   autoNormal $= Enabled
   normalize $= Enabled
   frontFace $= CW
   cullFace $= Just Back
   materialShininess Front $= 64
   return mbTexName

display ::  Maybe TextureObject -> DisplayCallback
display mbTexName = do
   clear [ ColorBuffer, DepthBuffer ]
   preservingMatrix $ do
      rotate (45 :: GLfloat) (Vector3 0 0 1)
      when (isJust mbTexName) $ textureBinding Texture1D $= mbTexName
      renderObject Solid (Teapot 2)
   flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   let wf = fromIntegral w
       hf = fromIntegral h
   if w <= h
      then ortho (-3.5) 3.5 (-3.5*hf/wf) (3.5*hf/wf) (-3.5) 3.5
      else ortho (-3.5*wf/hf) (3.5*wf/hf) (-3.5) 3.5 (-3.5) 3.5
   matrixMode $= Modelview 0
   loadIdentity

keyboard :: KeyboardMouseCallback
keyboard (Char c) Down _ _ = case toLower c of
   'e'   -> setGenMode EyeLinear
   'o'   -> setGenMode ObjectLinear
   's'   -> setPlane slanted
   'x'   -> setPlane xEqualZero
   '\27' -> exitWith ExitSuccess
   _     -> return ()
keyboard _ _ _ _ = return ()

setGenMode :: (Plane GLdouble -> TextureGenMode) -> IO ()
setGenMode mode = do
   currentGenMode <- get (textureGenMode S)
   case currentGenMode of
      Just (EyeLinear    plane) -> textureGenMode S $= Just (mode plane)
      Just (ObjectLinear plane) -> textureGenMode S $= Just (mode plane)
      _ -> error "setGenMode: should never happen..."
   postRedisplay Nothing

setPlane :: Plane GLdouble -> IO ()
setPlane plane = do
   currentGenMode <- get (textureGenMode S)
   case currentGenMode of
      Just (EyeLinear    _) -> textureGenMode S $= Just (EyeLinear    plane)
      Just (ObjectLinear _) -> textureGenMode S $= Just (ObjectLinear plane)
      _ -> error "setPlane: should never happen..."
   postRedisplay Nothing

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode, WithDepthBuffer ]
   initialWindowSize $= Size 256 256
   initialWindowPosition $= Position 100 100
   createWindow progName
   mbTexName <- myInit
   displayCallback $= display mbTexName
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just keyboard
   mainLoop
