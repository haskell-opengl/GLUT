{-
   OpenGLApplication.hs (adapted from OpenGLApplication which is (c) 2004 Astle/Hawkins)
   Copyright (c) Sven Panne 2004-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE
-}

import Control.Monad ( when, unless )
import Data.IORef ( IORef, newIORef )
import Data.Maybe ( isJust )
import Graphics.UI.GLUT hiding ( initialize )
import System.Console.GetOpt
import System.Environment ( getProgName )
import System.Exit ( exitWith, ExitCode(..) )
import System.IO ( hPutStr, stderr )

--------------------------------------------------------------------------------
-- Setup GLUT and OpenGL, drop into the event loop.
--------------------------------------------------------------------------------
main :: IO ()
main = do
   -- Setup the basic GLUT stuff
   (_, args) <- getArgsAndInitialize
   opts <- parseOptions args
   initialDisplayMode $= [ DoubleBuffered, RGBMode, WithDepthBuffer ]
   (if useFullscreen opts then fullscreenMode else windowedMode) opts

   state <- initialize

   -- Register the event callback functions
   displayCallback $= do render state; swapBuffers
   reshapeCallback $= Just setupProjection
   keyboardMouseCallback $= Just keyboardMouseHandler
   idleCallback $= Just (do prepare state; postRedisplay Nothing)

   -- At this point, control is relinquished to the GLUT event handler.
   -- Control is returned as events occur, via the callback functions.
   mainLoop

fullscreenMode :: Options -> IO ()
fullscreenMode opts = do
   let addCapability c = maybe id (\x -> (Where' c IsEqualTo x :))
   gameModeCapabilities $=
      (addCapability GameModeWidth (Just (windowWidth  opts)) .
       addCapability GameModeHeight (Just (windowHeight opts)) .
       addCapability GameModeBitsPerPlane (bpp opts) .
       addCapability GameModeRefreshRate (refreshRate opts)) []
   enterGameMode
   maybeWin <- get currentWindow
   if isJust maybeWin
      then cursor $= None
      else do
         hPutStr stderr "Could not enter fullscreen mode, using windowed mode\n"
         windowedMode (opts { useFullscreen = False } )

windowedMode :: Options -> IO ()
windowedMode opts = do
   initialWindowSize $=
      Size (fromIntegral (windowWidth opts)) (fromIntegral (windowHeight opts))
   createWindow "BOGLGP - Chapter 2 - OpenGL Application"
   return ()

--------------------------------------------------------------------------------
-- Option handling
--------------------------------------------------------------------------------
data Options = Options {
   useFullscreen :: Bool,
   windowWidth   :: Int,
   windowHeight  :: Int,
   bpp           :: Maybe Int,
   refreshRate   :: Maybe Int
   }

startOpt :: Options
startOpt = Options {
   useFullscreen = False,
   windowWidth   = 800,
   windowHeight  = 600,
   bpp           = Nothing,
   refreshRate   = Nothing
   }

options :: [OptDescr (Options -> IO Options)]
options = [
   Option ['f'] ["fullscreen"]
      (NoArg (\opt -> return opt { useFullscreen = True }))
      "use fullscreen mode if possible",
   Option ['w'] ["width"]
      (ReqArg (\arg opt -> do w <- readInt "WIDTH" arg
                              return opt { windowWidth = w })
              "WIDTH")
      "use window width WIDTH",
   Option ['h'] ["height"]
      (ReqArg (\arg opt -> do h <- readInt "HEIGHT" arg
                              return opt { windowHeight = h })
              "HEIGHT")
      "use window height HEIGHT",
   Option ['b'] ["bpp"]
      (ReqArg (\arg opt -> do b <- readInt "BPP" arg
                              return opt { bpp = Just b })
              "BPP")
      "use BPP bits per plane (ignored in windowed mode)",
   Option ['r'] ["refresh-rate"]
      (ReqArg (\arg opt -> do r <- readInt "HZ" arg
                              return opt { refreshRate = Just r })
              "HZ")
      "use refresh rate HZ (ignored in windowed mode)",
   Option ['?'] ["help"]
      (NoArg (\_ -> do usage >>= putStr
                       safeExitWith ExitSuccess))
      "show help" ]

readInt :: String -> String -> IO Int
readInt name arg =
   case reads arg of
      ((x,[]) : _) -> return x
      _ -> dieWith ["Can't parse " ++ name ++ " argument '" ++ arg ++ "'\n"]

usage :: IO String
usage = do
   progName <- getProgName
   return $ usageInfo ("Usage: " ++ progName ++ " [OPTION...]") options

parseOptions :: [String] -> IO Options
parseOptions args = do
   let (optsActions, nonOptions, errs) = getOpt Permute options args
   unless (null nonOptions && null errs) (dieWith errs)
   foldl (>>=) (return startOpt) optsActions

dieWith :: [String] -> IO a
dieWith errs = do
   u <- usage
   mapM_ (hPutStr stderr) (errs ++ [u])
   safeExitWith (ExitFailure 1)

--------------------------------------------------------------------------------
-- Handle mouse and keyboard events. For this simple demo, just exit when
-- ESCAPE is pressed.
--------------------------------------------------------------------------------
keyboardMouseHandler :: KeyboardMouseCallback
keyboardMouseHandler (Char '\27') Down _ _ = safeExitWith ExitSuccess
keyboardMouseHandler _             _   _ _ = return ()

safeExitWith :: ExitCode -> IO a
safeExitWith code = do
    gma <- get gameModeActive
    when gma leaveGameMode
    exitWith code

--------------------------------------------------------------------------------
-- The globale state, which is only the current angle in this simple demo.
-- We don't need the window dimensions here, they are not used and would
-- be easily available via GLUT anyway.
--------------------------------------------------------------------------------
data State = State { angle :: IORef GLfloat }

makeState :: IO State
makeState = do
   a <- newIORef 0
   return $ State { angle = a }

--------------------------------------------------------------------------------
-- Do one time setup, i.e. set the clear color and create the global state.
--------------------------------------------------------------------------------
initialize :: IO State
initialize = do
   -- clear to black background
   clearColor $= Color4 0 0 0 0

   makeState

--------------------------------------------------------------------------------
-- Reset the viewport for window changes.
--------------------------------------------------------------------------------
setupProjection :: ReshapeCallback
setupProjection (Size width height) = do
   -- don't want a divide by zero
   let h = max 1 height
   -- reset the viewport to new dimensions
   viewport $= (Position 0 0, Size width h)
   -- set projection matrix as the current matrix
   matrixMode $= Projection
   -- reset projection matrix
   loadIdentity

   -- calculate aspect ratio of window
   perspective 52 (fromIntegral width / fromIntegral h) 1 1000

   -- set modelview matrix
   matrixMode $= Modelview 0
   -- reset modelview matrix
   loadIdentity

--------------------------------------------------------------------------------
-- Perform any data-specific updates for a frame. Here we only increment the
-- angle for the rotation of the triangle.
--------------------------------------------------------------------------------
prepare :: State -> IdleCallback
prepare state = do
   angle state $~ (+ 0.1)

--------------------------------------------------------------------------------
-- Clear and redraw the scene.
--------------------------------------------------------------------------------
render :: State -> DisplayCallback
render state = do
   -- clear screen and depth buffer
   clear [ ColorBuffer, DepthBuffer ]
   loadIdentity

   -- resolve overloading, not needed in "real" programs
   let translate3f = translate :: Vector3 GLfloat -> IO ()
       color3f = color :: Color3 GLfloat -> IO ()
       vertex3f = vertex :: Vertex3 GLfloat -> IO ()

   -- move back 5 units and rotate about all 3 axes
   translate3f (Vector3 0 0 (-5))
   a <- get (angle state)
   rotate a (Vector3 1 0 0)
   rotate a (Vector3 0 1 0)
   rotate a (Vector3 0 0 1)

   -- lime greenish color
   color3f (Color3 0.7 1 0.3)

   -- draw the triangle such that the rotation point is in the center
   renderPrimitive Triangles $ do
      vertex3f (Vertex3   1  (-1) 0)
      vertex3f (Vertex3 (-1) (-1) 0)
      vertex3f (Vertex3   0    1  0)
