{-
   Gears.hs (adapted from gears.c which is (c) Brian Paul)
   Copyright (c) Shawn P. Garbett 2004 <shawn@garbett.org>
   Further hacked by Sven Panne <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   Command line options:
     -info      print GL implementation information
     -exit      automatically exit after 30 seconds
-}

--------------------------------------------------------------------------------

import Control.Monad ( when )
import Data.IORef ( IORef, newIORef )
import Data.List ( intersperse )
import System.Console.GetOpt
import System.Exit ( exitWith, ExitCode(ExitSuccess), exitFailure )
import Graphics.UI.GLUT

--------------------------------------------------------------------------------

type View = (GLfloat, GLfloat, GLfloat)

data State = State {
   frames  :: IORef Int,
   t0      :: IORef Int,
   viewRot :: IORef View,
   angle'  :: IORef GLfloat }

makeState :: IO State
makeState = do
   f <- newIORef 0
   t <- newIORef 0
   v <- newIORef (20, 30, 0) 
   a <- newIORef 0
   return $ State { frames = f, t0 = t, viewRot = v, angle' = a }

--  Draw a gear wheel.  You'll probably want to call this function when
--  building a display list since we do a lot of trig here.
-- 
--  Input: innerRadius - radius of hole at center
--         outerRadius - radius at center of teeth
--         width - width of gear
--         teeth - number of teeth
--         toothDepth - depth of tooth

gear :: GLfloat -> GLfloat -> GLfloat -> GLint -> GLfloat -> IO ()
gear innerRadius outerRadius width teeth toothDepth = do
   let r0 = innerRadius
       r1 = outerRadius - toothDepth / 2
       r2 = outerRadius + toothDepth / 2

       da = 2 * pi / fromIntegral teeth / 4
       w  = 0.5 * width

       render p f =
          renderPrimitive p $
             mapM_ (\i -> let angle = fromIntegral i * 2 * pi / fromIntegral teeth
                              in f r0 r1 r2 w da teeth i angle)
                   [ 0 .. teeth ]

   shadeModel $= Flat
   currentNormal $= Normal3 0 0 1
   render QuadStrip gearFront
   render Quads     teethFront
   currentNormal $= Normal3 0 0 (-1)
   render QuadStrip gearBack
   render Quads     teethBack
   render QuadStrip teethFace
   shadeModel $= Smooth
   render QuadStrip gearInside

type Renderer =
   GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLint -> GLint -> GLfloat -> IO ()

-- draw front face
gearFront :: Renderer
gearFront r0 r1 _ w da teeth i angle = do
   vertex (Vertex3 (r0 * cos angle) (r0 * sin angle) w)
   vertex (Vertex3 (r1 * cos angle) (r1 * sin angle) w)
   when (i < teeth) $ do
      vertex (Vertex3 (r0 * cos  angle          ) (r0 * sin  angle          ) w)
      vertex (Vertex3 (r1 * cos (angle + 3 * da)) (r1 * sin (angle + 3 * da)) w)

-- draw front sides of teeth
teethFront :: Renderer
teethFront _ r1 r2 w da teeth i angle =
   when (i < teeth) $ do
      vertex (Vertex3 (r1 * cos  angle          ) (r1 * sin  angle          ) w)
      vertex (Vertex3 (r2 * cos (angle +     da)) (r2 * sin (angle +     da)) w)
      vertex (Vertex3 (r2 * cos (angle + 2 * da)) (r2 * sin (angle + 2 * da)) w)
      vertex (Vertex3 (r1 * cos (angle + 3 * da)) (r1 * sin (angle + 3 * da)) w)

-- draw back face
gearBack :: Renderer
gearBack r0 r1 _ w da teeth i angle = do 
   vertex (Vertex3 (r1 * cos angle) (r1 * sin angle) (-w))
   vertex (Vertex3 (r0 * cos angle) (r0 * sin angle) (-w))
   when (i < teeth) $ do
      vertex (Vertex3 (r1 * cos (angle + 3 * da)) (r1 * sin (angle + 3 * da)) (-w))
      vertex (Vertex3 (r0 * cos  angle          ) (r0 * sin  angle          ) (-w))

-- draw back sides of teeth
teethBack :: Renderer
teethBack _ r1 r2 w da teeth i angle =
   when (i < teeth) $ do
      vertex (Vertex3 (r1 * cos (angle + 3 * da)) (r1 * sin (angle + 3 * da)) (-w))
      vertex (Vertex3 (r2 * cos (angle + 2 * da)) (r2 * sin (angle + 2 * da)) (-w))
      vertex (Vertex3 (r2 * cos (angle +     da)) (r2 * sin (angle +     da)) (-w))
      vertex (Vertex3 (r1 * cos  angle          ) (r1 * sin  angle          ) (-w))

-- draw outward faces of teeth
teethFace :: Renderer
teethFace _ r1 r2 w da teeth i angle =
  if (i < teeth) then do
    vertex (Vertex3 (r1*(cos angle)) (r1*(sin angle)) w)
    vertex (Vertex3 (r1*(cos angle)) (r1*(sin angle)) (-w))

    let u'    = r2 * cos (angle + da) - r1 * cos angle
        v'    = r2 * sin (angle + da) - r1 * sin angle
        len   = sqrt (u' * u' + v' * v')
        u     = u' / len
        v     = v' / len
    currentNormal $= Normal3 v (-u) 0
    vertex (Vertex3 (r2 * cos (angle +     da)) (r2 * sin (angle +     da))   w )
    vertex (Vertex3 (r2 * cos (angle +     da)) (r2 * sin (angle +     da)) (-w))
    currentNormal $= Normal3 (cos angle) (sin angle) 0
    vertex (Vertex3 (r2 * cos (angle + 2 * da)) (r2 * sin (angle + 2 * da))   w )
    vertex (Vertex3 (r2 * cos (angle + 2 * da)) (r2 * sin (angle + 2 * da)) (-w))
    let u2    = r1 * cos (angle + 3 * da) - r2 * cos (angle + 2 * da);
        v2    = r1 * sin (angle + 3 * da) - r2 * sin (angle + 2 * da);
    currentNormal $= Normal3 v2 (-u2) 0
    vertex (Vertex3 (r1 * cos (angle + 3 * da)) (r1 * sin (angle + 3 * da))   w )
    vertex (Vertex3 (r1 * cos (angle + 3 * da)) (r1 * sin (angle + 3 * da)) (-w))
    currentNormal $= Normal3 (cos angle) (sin angle) 0
  else do
    vertex (Vertex3 (r1 * cos 0) (r1 * sin 0)   w )
    vertex (Vertex3 (r1 * cos 0) (r1 * sin 0) (-w))

-- draw inside radius cylinder
gearInside :: Renderer
gearInside r0 _ _ w _ _ _ angle = do
   currentNormal $= Normal3 (-cos angle) (-sin angle) 0
   vertex (Vertex3 (r0 * cos angle) (r0 * sin angle) (-w))
   vertex (Vertex3 (r0 * cos angle) (r0 * sin angle)   w )
     
draw :: (DisplayList,DisplayList,DisplayList,Int) -> State -> IO ()
draw (gear1, gear2, gear3, autoexit) state = do
   clear [ ColorBuffer, DepthBuffer ]
   (x, y, z) <- get (viewRot state)
   a <- get (angle' state)

   let translatef = translate :: Vector3 GLfloat -> IO ()
   preservingMatrix $ do
      rotate x (Vector3 1 0 0)
      rotate y (Vector3 0 1 0)
      rotate z (Vector3 0 0 1)

      preservingMatrix $ do
         translatef (Vector3 (-3) (-2) 0)
         rotate a (Vector3 0 0 1)
         callList gear1

      preservingMatrix $ do
         translatef (Vector3 3.1 (-2) 0)
         rotate (-2 * a - 9) (Vector3 0 0 1)
         callList gear2

      preservingMatrix $ do
         translatef (Vector3 (-3.1) 4.2 0)
         rotate (-2 * a - 25) (Vector3 0 0 1)
         callList gear3

   swapBuffers  
   frames state $~! (+1)
   t0' <- get (t0 state)
   t <- get elapsedTime
   when (t - t0' >= 5000) $ do
      f <- get (frames state)
      let seconds = fromIntegral (t - t0') / 1000 :: GLfloat
          fps = fromIntegral f / seconds
      putStrLn (show f ++ " frames in " ++ show seconds ++ " seconds = "++ show fps ++ " FPS")
      t0 state $= t
      frames state $= 0
      when ((t >= 999 * autoexit) && (autoexit /= 0)) $
         exitWith ExitSuccess

idle :: State -> IdleCallback
idle state = do
   angle' state $~! (+2)
   postRedisplay Nothing

keyboard :: State -> KeyboardMouseCallback
keyboard state (Char 'z')           _ _ _ = modRot state ( 0,  0,  5)
keyboard state (Char 'Z')           _ _ _ = modRot state ( 0,  0, -5)
keyboard state (SpecialKey KeyUp)   _ _ _ = modRot state ( 5,  0,  0)
keyboard state (SpecialKey KeyDown) _ _ _ = modRot state (-5,  0,  0)
keyboard state (SpecialKey KeyLeft) _ _ _ = modRot state ( 0,  5,  0)
keyboard state (SpecialKey KeyRight)_ _ _ = modRot state ( 0, -5,  0)
keyboard _     (Char '\27')         _ _ _ = exitWith ExitSuccess
keyboard _     _                    _ _ _ = return ()

modRot :: State -> View -> IO ()
modRot state (dx,dy,dz) = do
   (x, y, z) <- get (viewRot state)
   viewRot state $= (x + dx, y + dy, z + dz)
   postRedisplay Nothing

-- new window size or exposure
reshape :: ReshapeCallback
reshape s@(Size width height) = do
   let h = fromIntegral height / fromIntegral width

   viewport $= (Position 0 0, s)
   matrixMode $= Projection
   loadIdentity
   frustum (-1) 1 (-h) h 5 60
   matrixMode $= Modelview 0
   loadIdentity
   translate (Vector3 0 0 (-40 :: GLfloat))

data Flag = GLInfo | AutoExit deriving ( Eq, Ord, Show )

argInfo :: [OptDescr Flag]
argInfo  = [
   Option ['i'] ["info"] (NoArg GLInfo)   "print gl information",
   Option ['e'] ["exit"] (NoArg AutoExit) "auto exit after 30 seconds" ]

opts :: [String] -> IO [Flag]
opts args = 
   case getOpt Permute argInfo args of
      (o,_,[])   -> return o
      (_,_,errs) -> do
         putStr (concat errs ++ usageInfo "Usage: Gears [OPTION...]" argInfo)
         exitFailure

info :: IO ()
info = do
   rendererStr <- get renderer
   putStrLn ("GL_RENDERER   = " ++ rendererStr)
   vendorStr <- get vendor
   putStrLn ("GL_VENDOR     = " ++ vendorStr)
   versionStr <- get glVersion
   putStrLn ("GL_VERSION    = " ++ versionStr)
   exts <- get glExtensions
   putStrLn ("GL_EXTENSIONS = " ++ concat (intersperse " " exts))

myInit :: [String] -> IO (DisplayList,DisplayList,DisplayList,Int)
myInit args = do
   position (Light 0) $= Vertex4 5 5 10 0
   cullFace $= Just Back
   lighting $= Enabled
   light (Light 0) $= Enabled
   depthFunc $= Just Less

   -- make the gears
   g1 <- defineNewList Compile $ do
      materialAmbientAndDiffuse Front $= Color4 0.8 0.1 0.0 1.0
      gear 1 4 1 20 0.7

   g2 <- defineNewList Compile $ do
      materialAmbientAndDiffuse Front $= Color4 0.0 0.8 0.2 1.0
      gear 0.5 2 2 10 0.7

   g3 <- defineNewList Compile $ do
      materialAmbientAndDiffuse Front $= Color4 0.2 0.2 1.0 1.0
      gear 1.3 2 0.5 10 0.7

   normalize $= Enabled

   flags <- opts args
   when (GLInfo `elem` flags) info
   let autoexit = if AutoExit `elem` flags then 30 else 0
   when (autoexit /= 0) $
      putStrLn ("Auto Exit after " ++ show autoexit ++ " seconds.")

   return (g1, g2, g3, autoexit)

visible :: State -> Visibility -> IO ()
visible state Visible    = idleCallback $= Just (idle state)
visible _     NotVisible = idleCallback $= Nothing

main :: IO ()
main  = do
   (_progName, args) <- getArgsAndInitialize
   initialDisplayMode $= [ RGBMode, WithDepthBuffer, DoubleBuffered ]

   initialWindowPosition $= Position 0 0
   initialWindowSize $= Size 300 300
   createWindow "Gears"
   state <- makeState
   gearsAndAuto <- myInit args

   displayCallback $= draw gearsAndAuto state
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just (keyboard state)
   visibilityCallback $= Just (visible state)

   mainLoop
