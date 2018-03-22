{-# LANGUAGE CPP #-}
{-
   Brick.hs (adapted from ogl2brick.c which is (c) 3Dlabs Inc. Ltd.)
   Copyright (c) Sven Panne 2018 <svenpanne@gmail.com>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE
-}

#if !MIN_VERSION_base(4,8,0)
import Prelude hiding ( sum )
import Data.Foldable ( Foldable, sum )
#endif
import Control.Applicative
import Control.Exception ( IOException, catch )
import Control.Monad ( when, unless )
import qualified Data.ByteString as B
import Data.IORef
import System.Exit
import Graphics.UI.GLUT

infixl 6 $+, $-
infixl 7 $*

inertiaThreshold, inertiaFactor :: GLfloat
inertiaThreshold = 1
inertiaFactor = 0.5

scaleFactor, scaleIncrement :: GLfloat
scaleFactor = 0.01
scaleIncrement = 0.5

timerFrequencyMillis :: Timeout
timerFrequencyMillis = 20

clearColors :: [Color4 GLclampf]
clearColors = [
   Color4 0.0 0.0 0.0 1,
   Color4 0.2 0.2 0.3 1,
   Color4 0.7 0.7 0.7 1 ]

models :: [IO ()]
models = [
   drawCube,
   renderObject Solid (Teapot 0.6),
   renderObject Solid (Sphere' 0.6 64 64),
   renderObject Solid (Torus 0.2 0.6 64 64) ]

initialDiff :: Vector3 GLfloat
initialDiff = Vector3 206 16 10

initialInertia :: Vector3 GLfloat
initialInertia = Vector3 (-0.5) 0 0

data State = State {
   diff :: IORef (Vector3 GLfloat),
   lastIncr :: IORef (Vector3 GLfloat),
   inertia :: IORef (Vector3 GLfloat),
   inertiaOld :: IORef (Vector3 GLfloat),
   theScale :: IORef GLfloat,
   lastPosition :: IORef Position,
   shouldRotate :: IORef Bool,
   colorCycle :: IORef [Color4 GLclampf],
   modelCycle :: IORef [IO ()],
   modifiers :: IORef Modifiers
   }

makeState :: IO State
makeState = do
   di <- newIORef initialDiff
   li <- newIORef (pure 0)
   ia <- newIORef initialInertia
   io <- newIORef (pure 0)
   sc <- newIORef 1
   lp <- newIORef (Position (-1) (-1))
   sr <- newIORef True
   cc <- newIORef (cycle clearColors)
   mc <- newIORef (cycle models)
   mo <- newIORef (Modifiers Up Up Up)
   return $ State {
      diff = di,
      lastIncr = li,
      inertia = ia,
      inertiaOld = io,
      theScale = sc,
      lastPosition = lp,
      shouldRotate = sr,
      colorCycle = cc,
      modelCycle = mc,
      modifiers = mo
      }

-- Our tiny vector math library...
($+), ($-), ($*) :: (Applicative t, Num a) => t a -> t a -> t a
($+) = liftA2 (+)
($-) = liftA2 (-)
($*) = liftA2 (*)

step :: (Applicative t, Num a, Ord a) => t a -> t a -> t a
step = liftA2 (\e x -> if x < e then 0 else 1)

dot :: (Applicative t, Foldable t, Num a) => t a -> t a -> a
dot v1 v2 = sum (v1 $* v2)

drawFace :: Normal3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat
         -> Vertex3 GLfloat -> Vertex3 GLfloat -> IO ()
drawFace p q r s t = do
   let texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()
   normal p
   texCoord2f (TexCoord2 1 1)
   vertex q
   texCoord2f (TexCoord2 0 1)
   vertex r
   texCoord2f (TexCoord2 0 0)
   vertex s
   texCoord2f (TexCoord2 1 0)
   vertex t

drawCube :: IO ()
drawCube = do
   let size = 1
       sc = 0.2
       delta = 0.1

       a = Vertex3   size    size  ( size * sc + delta)
       b = Vertex3   size    size  (-size * sc + delta)
       c = Vertex3   size  (-size) (-size * sc)
       d = Vertex3   size  (-size) ( size * sc)
       e = Vertex3 (-size)   size  ( size * sc + delta)
       f = Vertex3 (-size)   size  (-size * sc + delta)
       g = Vertex3 (-size) (-size) (-size * sc)
       h = Vertex3 (-size) (-size) ( size * sc)

       i = Normal3   1    0    0
       k = Normal3 (-1)   0    0
       l = Normal3   0    0  (-1)
       m = Normal3   0    0    1
       n = Normal3   0    1    0
       o = Normal3   0  (-1)   0

   renderPrimitive Quads $ do
      drawFace i d c b a
      drawFace k g h e f
      drawFace l c g f b
      drawFace m h d a e
      drawFace n e a b f
      drawFace o g c d h

display :: State -> DisplayCallback
display state = do
   loadIdentity
   translate (Vector3 0 0 (-5 :: GLfloat))

   Vector3 xDiff yDiff zDiff <- get (diff state)
   rotate yDiff (Vector3 1 0 0)
   rotate xDiff (Vector3 0 1 0)
   rotate zDiff (Vector3 0 0 1)

   sc <- get (theScale state)
   scale sc sc sc

   clear [ ColorBuffer, DepthBuffer ]
   (drawModel:_) <- get (modelCycle state)
   drawModel

   flush
   swapBuffers

nextClearColor :: State -> IO ()
nextClearColor state = do
   cc <- get (colorCycle state)
   clearColor $= head cc
   colorCycle state $~ tail

toggleRotation :: State -> IO ()
toggleRotation state = do
   rot <- get (shouldRotate state)
   shouldRotate state $~ not
   if rot
      then do
         ia <- get (inertia state)
         inertiaOld state $= ia
      else do
        io <- get (inertiaOld state)
        inertia state $= io
        -- To prevent confusion, force some rotation
        when (dot io io == 0) $
           inertia state $= initialInertia

printHelp :: IO ()
printHelp = mapM_ putStrLn [
   "",
   "Keyboard commands:",
   "",
   "b - Toggle among background clear colors",
   "q, <esc> - Quit",
   "t - Toggle among models to render",
   "? - Help",
   "<home> - reset zoom and rotation",
   "<space> or <click> - stop rotation",
   "<+>, <-> or <ctrl + drag> - zoom model",
   "<arrow keys> or <drag> - rotate model",
   ""]

resetState :: State -> IO ()
resetState state = do
   diff state $= initialDiff
   lastIncr state $= pure 0
   inertia state $= initialInertia
   theScale state $= 1

calcInertia :: State -> IO ()
calcInertia state = do
   lastPosition state $= Position (-1) (-1)
   li <- get (lastIncr state)
   ia <- get (inertia state)
   let t = pure inertiaThreshold
       f = pure inertiaFactor
       l = (pure 1 $- (step (fmap negate t) li)) $* ((li $+ t) $* f $- ia)
       r = (step t li) $* ((li $- t) $* f $- ia)
   inertia state $= l $+ ia $+ r
   lastIncr state $= pure 0

keyboard :: State -> KeyboardMouseCallback
keyboard state key keyState mods _ = do
   modifiers state $= mods
   postRedisplay Nothing
   case (key, keyState) of
      (Char 'b', Down) -> nextClearColor state
      (Char 'q', Down) -> exitWith ExitSuccess
      (Char '\27', Down) -> exitWith ExitSuccess
      (Char 't', Down) -> modelCycle state $~ tail
      (Char ' ', Down) -> toggleRotation state
      (Char '+', Down) -> theScale state $~ (+ scaleIncrement)
      (Char '-', Down) -> theScale state $~ (+ (- scaleIncrement))
      (Char _, Down) -> printHelp
      (SpecialKey KeyHome, Down) -> resetState state
      (SpecialKey KeyLeft, Down) -> diff state $~ ($- Vector3 1 0 0)
      (SpecialKey KeyRight, Down) -> diff state $~ ($+ Vector3 1 0 0)
      (SpecialKey KeyUp, Down) -> diff state $~ ($- Vector3 0 1 0)
      (SpecialKey KeyDown, Down) -> diff state $~ ($+ Vector3 0 1 0)
      (MouseButton LeftButton, Down) -> do
         inertia state $= pure 0
         lastIncr state $= pure 0
      (MouseButton LeftButton, Up) -> calcInertia state
      (_, _) -> return ()

motion :: State -> MotionCallback
motion state pos@(Position x y) = do
   postRedisplay Nothing
   Position xt yt <- get (lastPosition state)
   lastPosition state $= pos
   when (xt /= -1 || yt /= -1) $ do
      let li@(Vector3 xl yl _) = Vector3 (fromIntegral (x - xt)) (fromIntegral (y - yt)) 0
      lastIncr state $= li
      when (xt /= -1) $ do
         mods <- get (modifiers state)
         if ctrl mods == Down
            then do diff state $~ ($+ Vector3 0 0 xl)
                    theScale state $~ (+ (yl * scaleFactor))
            else diff state $~ ($+ li)

timer :: State -> TimerCallback
timer state = do
   rot <- get (shouldRotate state)
   when rot $ do
      ia <- get (inertia state)
      diff state $~ ($+ ia)
      postRedisplay Nothing
   addTimerCallback timerFrequencyMillis (timer state)

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   let vp = 0.8
       aspect = fromIntegral w / fromIntegral h

   viewport $= (Position 0 0, size)

   matrixMode $= Projection
   loadIdentity
   frustum (-vp) vp (-vp / aspect) (vp / aspect) 3 10

   matrixMode $= Modelview 0
   loadIdentity
   translate (Vector3 0 0 (-5 :: GLfloat))

-- Make sure that GLSL is supported by the driver, either directly by the core
-- or via an extension.
checkGLSLSupport :: IO ()
checkGLSLSupport = do
   version <- get (majorMinor glVersion)
   unless (version >= (2,0)) $ do
      extensions <- get glExtensions
      unless ("GL_ARB_shading_language_100" `elem` extensions) $
         ioError (userError "No GLSL support found.")

readAndCompileShader :: ShaderType -> FilePath -> IO Shader
readAndCompileShader st filePath = do
   src <- B.readFile filePath
   shader <- createShader st
   shaderSourceBS shader $= src
   compileShader shader
   reportErrors
   ok <- get (compileStatus shader)
   infoLog <- get (shaderInfoLog shader)
   mapM_ putStrLn ["Shader info log for '" ++ filePath ++ "':", infoLog, ""]
   unless ok $ do
      deleteObjectNames [shader]
      ioError (userError "shader compilation failed")
   return shader

installBrickShaders :: [Shader] -> IO ()
installBrickShaders shaders = do
   brickProg <- createProgram
   attachedShaders brickProg $= shaders
   linkProgram brickProg
   reportErrors
   ok <- get (linkStatus brickProg)
   infoLog <- get (programInfoLog brickProg)
   mapM_ putStrLn ["Program info log:", infoLog, ""]
   unless ok $ do
      deleteObjectNames [brickProg]
      ioError (userError "linking failed")

   currentProgram $= Just brickProg

   let setUniform var val = do
          location <- get (uniformLocation brickProg var)
          reportErrors
          uniform location $= val

   setUniform "BrickColor" (Color3 1.0 0.3 (0.2 :: GLfloat))
   setUniform "MortarColor" (Color3 0.85 0.86 (0.84 :: GLfloat))
   setUniform "BrickSize" (Vertex2 0.30 (0.15 :: GLfloat))
   setUniform "BrickPct" (Vertex2 0.90 (0.85 :: GLfloat))
   setUniform "LightPosition" (Vertex3 0 0 (4 :: GLfloat))

main :: IO ()
main = do
   _ <- getArgsAndInitialize
   initialDisplayMode $= [ RGBMode, WithDepthBuffer, DoubleBuffered ]
   initialWindowSize $= Size 500 500
   _ <- createWindow "3Dlabs Brick Shader"

   -- Note: We don't use an idle callback, we redisplay more intelligently.
   state <- makeState
   displayCallback $= display state
   keyboardMouseCallback $= Just (keyboard state)
   reshapeCallback $= Just reshape
   motionCallback $= Just (motion state)
   addTimerCallback timerFrequencyMillis (timer state)

   Control.Exception.catch
     (do checkGLSLSupport
         vs <- readAndCompileShader VertexShader "Brick.vert"
         fs <- readAndCompileShader FragmentShader "Brick.frag"
         installBrickShaders [vs, fs])
     (\exception -> do
         print (exception :: IOException)
         putStrLn "Using fixed function pipeline."
         materialDiffuse Front $= Color4 1 0.3 0.2 1
         materialSpecular Front $= Color4 0.3 0.3 0.3 1
         materialShininess Front $= 16
         position (Light 0) $= Vertex4 0 0 4 0
         lighting $= Enabled
         light (Light 0) $= Enabled)

   depthFunc $= Just Less
   nextClearColor state

   -- display help
   keyboard state (Char '?') Down (Modifiers Up Up Up) (Position 0 0)

   mainLoop
