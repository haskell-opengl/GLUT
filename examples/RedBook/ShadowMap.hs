{-
   ShadowMap.hs (adapted from shadowmap.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE
-}

import Control.Monad ( when, unless )
import Data.IORef ( IORef, newIORef )
import Foreign.Marshal.Array ( allocaArray )
import Foreign.Ptr ( nullPtr )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

shadowMapSize :: TextureSize2D
shadowMapSize = TextureSize2D 256 256

fovy, nearPlane, farPlane :: GLdouble
fovy      =  60
nearPlane =  10
farPlane  = 100

lightPos :: Vertex4 GLfloat
lightPos = Vertex4 25 25 25 1

lookat :: Vertex3 GLdouble
lookat   = Vertex3 0 0 0

up :: Vector3 GLdouble
up = Vector3 0 0 1

data State = State {
   angle      :: IORef GLdouble,
   torusAngle :: IORef GLfloat,
   showShadow :: IORef Bool,
   animate    :: IORef Bool,
   funcMode   :: IORef ComparisonFunction }

makeState :: IO State
makeState = do
   a <- newIORef 0
   t <- newIORef 0
   s <- newIORef False
   n <- newIORef True
   f <- newIORef Lequal
   return $ State { angle = a, torusAngle = t, showShadow = s,
                    animate = n, funcMode = f }

myInit :: IO ()
myInit = do
   texImage2D Nothing NoProxy 0 DepthComponent' shadowMapSize 0
              (PixelData DepthComponent UnsignedByte nullPtr)

   position (Light 0) $= lightPos
   let white = Color4 1 1 1 1
   specular (Light 0) $= white
   diffuse  (Light 0) $= white

   textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
   textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
   textureFilter Texture2D $= ((Linear', Nothing), Linear')
   textureCompareMode Texture2D $= Just Lequal
   depthTextureMode Texture2D $= Luminance'

   colorMaterial $= Just (FrontAndBack, AmbientAndDiffuse)

   cullFace $= Just Back

   depthFunc $= Just Less
   light (Light 0) $= Enabled
   lighting $= Enabled
   texture Texture2D $= Enabled

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   perspective fovy (fromIntegral w / fromIntegral h) nearPlane farPlane
   matrixMode $= Modelview 0

idle :: State -> IdleCallback
idle state = do
   angle state $~! (+ (pi / 10000))
   torusAngle state $~! (+ 0.1)
   postRedisplay Nothing

keyboard :: State -> KeyboardMouseCallback
keyboard state (Char c) Down _ _ = do
   case c of
      '\27' -> exitWith ExitSuccess
      't' ->
         texture Texture2D $~ \cap -> if cap == Enabled then Disabled else Enabled
      'm' -> do
         fm <- get (funcMode state)
         textureCompareMode Texture2D $~ maybe (Just fm) (const Nothing)
         compareMode <- get (textureCompareMode Texture2D)
         putStrLn ("Compare mode " ++ maybe "Off" (const "On") compareMode)
      'f' -> do
         funcMode state $~ \fm -> if fm == Lequal then Gequal else Lequal
         fm <- get (funcMode state)
         putStrLn ("Operator " ++ show fm)
         textureCompareMode Texture2D $~ maybe Nothing (const (Just fm))
      's' -> showShadow state $~ not
      'p' -> do
         animate state $~ not
         animate' <- get (animate state)
         idleCallback $= if animate' then Just (idle state) else Nothing
      _ -> return ()
   postRedisplay Nothing
keyboard _ _ _ _ _ = return ()

drawObjects :: GLfloat -> Bool -> IO ()
drawObjects torusAngle' shadowRender = do
   textureOn <- get (texture Texture2D)

   when shadowRender $
      texture Texture2D $= Disabled

   -- resolve overloading, not needed in "real" programs
   let normal3f = normal :: Normal3 GLfloat -> IO ()
       color3f = color :: Color3 GLfloat -> IO ()
       rectf = rect :: Vertex2 GLfloat -> Vertex2 GLfloat -> IO ()
       translatef = translate :: Vector3 GLfloat -> IO ()
       rotatef = rotate :: GLfloat -> Vector3 GLfloat -> IO ()

   unless shadowRender $ do
      normal3f (Normal3 0 0 1)
      color3f (Color3 1 1 1)
      rectf (Vertex2 (-20) (-20)) (Vertex2 20 20)
    
   preservingMatrix $ do
      translatef (Vector3 11 11 11)
      rotatef 54.73 (Vector3 (-5) 5 0)
      rotate torusAngle' (Vector3 1 0 0)
      color3f (Color3 1 0 0)
      renderObject Solid (Torus 1 4 8 36)

   preservingMatrix $ do
      translatef (Vector3 2 2 2)
      color3f (Color3 0 0 1)
      renderObject Solid (Cube 4)

   preservingMatrix $ do
      getLightPos Vector3 >>= translate
      color3f (Color3 1 1 1)
      renderObject Wireframe (Sphere' 0.5 6 6)

   when (shadowRender && textureOn == Enabled) $
      texture Texture2D $= Enabled

getLightPos :: (GLdouble -> GLdouble -> GLdouble -> a) -> IO a
getLightPos f = do
    Vertex4 x y z _ <- get (position (Light 0))
    return $ f (realToFrac x) (realToFrac y) (realToFrac z)

generateShadowMap :: GLfloat -> Bool -> IO ()
generateShadowMap torusAngle' showShadow' = do
   lightPos' <- getLightPos Vertex3
   let (TextureSize2D shadowMapWidth shadowMapHeight) = shadowMapSize
       shadowMapSize' = Size shadowMapWidth shadowMapHeight

   preservingViewport $ do
      viewport $= (Position 0 0, shadowMapSize')

      clear [ ColorBuffer, DepthBuffer ]

      matrixMode $= Projection
      preservingMatrix $ do
         loadIdentity
         perspective 80 1 10 1000
         matrixMode $= Modelview 0
         preservingMatrix $ do
            loadIdentity
            lookAt lightPos' lookat up
            drawObjects torusAngle' True
         matrixMode $= Projection
      matrixMode $= Modelview 0

      copyTexImage2D Nothing 0 DepthComponent' (Position 0 0) shadowMapSize 0

   when showShadow' $ do
      let numShadowMapPixels = fromIntegral (shadowMapWidth * shadowMapHeight)
      allocaArray numShadowMapPixels $ \depthImage -> do
        let pixelData fmt = PixelData fmt Float depthImage :: PixelData GLfloat
        readPixels (Position 0 0) shadowMapSize' (pixelData DepthComponent)
        (_, Size viewPortWidth _) <- get viewport
        windowPos (Vertex2 (fromIntegral viewPortWidth / 2 :: GLfloat) 0)
        drawPixels shadowMapSize' (pixelData Luminance)
        swapBuffers

-- Note: preservingViewport is not exception safe, but it doesn't matter here
preservingViewport :: IO a -> IO a
preservingViewport act = do
   v <- get viewport
   x <- act
   viewport $= v
   return x

generateTextureMatrix :: IO ()
generateTextureMatrix = do
   -- Set up projective texture matrix. We use the Modelview matrix stack and
   -- OpenGL matrix commands to make the matrix.
   m <- preservingMatrix $ do
      loadIdentity
      -- resolve overloading, not needed in "real" programs
      let translatef = translate :: Vector3 GLfloat -> IO ()
          scalef = scale :: GLfloat -> GLfloat -> GLfloat -> IO ()
      translatef (Vector3 0.5 0.5 0.0)
      scalef 0.5 0.5 1.0
      perspective 60 1 1 1000
      lightPos' <- getLightPos Vertex3
      lookAt lightPos' lookat up
      get (matrix (Just (Modelview 0)))

   [ sx, sy, sz, sw,
     tx, ty, tz, tw,
     rx, ry, rz, rw,
     qx, qy, qz, qw ] <- getMatrixComponents RowMajor (m :: GLmatrix GLdouble)

   textureGenMode S $= Just (ObjectLinear (Plane sx sy sz sw))
   textureGenMode T $= Just (ObjectLinear (Plane tx ty tz tw))
   textureGenMode R $= Just (ObjectLinear (Plane rx ry rz rw))
   textureGenMode Q $= Just (ObjectLinear (Plane qx qy qz qw))

display :: State -> DisplayCallback
display state = do
   let radius = 30
   torusAngle' <- get (torusAngle state)
   showShadow' <- get (showShadow state)
   generateShadowMap torusAngle' showShadow'
   generateTextureMatrix
   unless showShadow' $ do
      clear [ ColorBuffer, DepthBuffer ]
      preservingMatrix $ do
         angle' <- get (angle state)
         lookAt (Vertex3 (radius * cos angle') (radius * sin angle') 30) lookat up
         drawObjects torusAngle' False
      swapBuffers

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ RGBAMode, WithDepthBuffer, DoubleBuffered ]
   initialWindowSize $= Size 521 512
   initialWindowPosition $= Position 100 100
   createWindow progName
   state <- makeState
   myInit
   displayCallback $= display state
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just (keyboard state)
   idleCallback $= Just (idle state)

   mainLoop
