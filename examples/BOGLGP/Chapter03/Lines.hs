{-
   Lines.hs (adapted from Lines which is (c) 2004 Astle/Hawkins)
   Copyright (c) Sven Panne 2004-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE
-}

import Control.Monad ( when, unless, zipWithM_ )
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

   initialize

   -- Register the event callback functions
   displayCallback $= do render; swapBuffers
   reshapeCallback $= Just setupProjection
   keyboardMouseCallback $= Just keyboardMouseHandler
   -- No need for an idle callback here, this would just hog the CPU
   -- without any visible effect

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
   createWindow "BOGLGP - Chapter 3 - Lines"
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
-- Do one time setup, i.e. set the clear color.
--------------------------------------------------------------------------------
initialize :: IO ()
initialize = clearColor $= Color4 0 0 0 0

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
-- Clear and redraw the scene.
--------------------------------------------------------------------------------
render :: DisplayCallback
render = do
   -- clear screen and depth buffer
   clear [ ColorBuffer, DepthBuffer ]
   loadIdentity
   lookAt (Vertex3 0 10 0.1) (Vertex3 0 0 0) (Vector3 0 1 0)

   -- function to draw a series of lines of increasing width
   let drawLines x = zipWithM_ (\lw line -> do
          -- set the line width
          lineWidth $= lw

          -- draw the line
          renderPrimitive Lines $ do
             vertex (Vertex3  x      0 (line - 3))
             vertex (Vertex3 (x + 4) 0 (line - 3)))
          [ 0.5 .. ]
          [ 0, 0.5 .. 6.5 :: GLfloat ]

   -- draw a series of lines of increasing width without stippling
   drawLines (-5)

   -- set the stipple pattern, 0xAAAA = 1010 1010 1010 1010
   lineStipple $= Just (2, 0xAAAA)

   -- draw a series of lines of increasing width with stippling
   drawLines 1

   -- disable stippling
   lineStipple $= Nothing
