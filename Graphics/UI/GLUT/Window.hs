-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Window
-- Copyright   :  (c) Sven Panne 2002
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  experimental
-- Portability :  portable
--
-- GLUT supports two types of windows: top-level windows and subwindows. Both
-- types support OpenGL rendering and GLUT callbacks. There is a single
-- identifier space for both types of windows.
--
-----------------------------------------------------------------------------

module Graphics.UI.GLUT.Window (
   -- * Data types
   Window,

   -- * Functions
   createWindow, createSubWindow
) where

import Foreign.C.String ( CString, withCString )
import Foreign.C.Types ( CInt )
import Graphics.UI.GLUT.Initialization ( WindowPosition(..), WindowSize(..) )

-- | An opaque identifier for a top-level window or a subwindow.
newtype Window = Window CInt deriving ( Eq, Ord )

-- | Create a top-level window. The given name will be provided to the window
-- system as the window\'s name. The intent is that the window system will
-- label the window with the name.
--
-- Implicitly, the /current window/ is set to the newly created window.
--
-- Each created window has a unique associated OpenGL context. State
-- changes to a window\'s associated OpenGL context can be done
-- immediately after the window is created.
--
-- The /display state/ of a window is initially for the window to be
-- shown. But the window\'s /display state/ is not actually acted upon
-- until 'Graphics.UI.GLUT.Begin.mainLoop' is entered. This means until
-- 'Graphics.UI.GLUT.Begin.mainLoop' is called, rendering to a created
-- window is ineffective because the window can not yet be displayed.
--
-- The value returned is a unique identifier for the window, which can be
-- used when calling 'setWindow'.
--
-- X Implementation Notes: The proper X Inter-Client Communication
-- Conventions Manual (ICCCM) top-level properties are established. The
-- @WM_COMMAND@ property that lists the command line used to invoke the
-- GLUT program is only established for the first window created.

createWindow
   :: String    -- @ The window name
   -> IO Window -- @ The identifier for the newly created window
createWindow name = withCString name glutCreateWindow

foreign import ccall unsafe "glutCreateWindow" glutCreateWindow ::
      CString -> IO Window

-- | Create a subwindow of the identified window with the given relative
-- position and size. Implicitly, the /current window/ is set to the
-- newly created subwindow.
--
-- Each created window has a unique associated OpenGL context. State
-- changes to a window\'s associated OpenGL context can be done
-- immediately after the window is created.
--
-- The /display state/ of a window is initially for the window to be
-- shown. But the window\'s /display state/ is not actually acted upon
-- until 'Graphics.UI.GLUT.Begin.mainLoop' is entered. This means until
-- 'Graphics.UI.GLUT.Begin.mainLoop' is called, rendering to a created
-- window is ineffective. Subwindows can not be iconified.
--
-- Subwindows can be nested arbitrarily deep.

createSubWindow
   :: Window         -- @ Identifier of the subwindow\'s parent window.
   -> WindowPosition -- @ Window position in pixels relative to parent window\'s origin.
   -> WindowSize     -- @ Window size in pixels
   -> IO Window      -- @ The identifier for the newly created subwindow
createSubWindow win (WindowPosition x y) (WindowSize w h) =
   glutCreateSubWindow win x y w h

foreign import ccall unsafe "glutCreateSubWindow" glutCreateSubWindow ::
      Window -> CInt -> CInt -> CInt -> CInt -> IO Window
