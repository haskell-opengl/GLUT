-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.FreeGLUT
-- Copyright   :  (c) Sven Panne 2002-2004
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This purely internal module handles all freeglut specific stuff.
--
-----------------------------------------------------------------------------

module Graphics.UI.GLUT.FreeGLUT where

import Foreign.Ptr ( Ptr, FunPtr )
import Foreign.C.String ( CString )
import Foreign.C.Types ( CInt )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLint, GLenum, GLfloat, GLdouble )

-----------------------------------------------------------------------------
-- New glutGet parameters

glut_ACTION_ON_WINDOW_CLOSE, glut_WINDOW_BORDER_WIDTH,
   glut_WINDOW_HEADER_HEIGHT, glut_VERSION, glut_RENDERING_CONTEXT,
   glut_DIRECT_RENDERING :: GLenum
glut_ACTION_ON_WINDOW_CLOSE            = 505
glut_WINDOW_BORDER_WIDTH               = 506
glut_WINDOW_HEADER_HEIGHT              = 507
glut_VERSION                           = 508
glut_RENDERING_CONTEXT                 = 509
glut_DIRECT_RENDERING                  = 510

-----------------------------------------------------------------------------
-- Direct/indirect rendering context options (has meaning only in unix/x11),
-- see glut_DIRECT_RENDERING (freeglut extension)

glut_FORCE_INDIRECT_CONTEXT, glut_ALLOW_DIRECT_CONTEXT,
   glut_TRY_DIRECT_CONTEXT, glut_FORCE_DIRECT_CONTEXT :: CInt
glut_FORCE_INDIRECT_CONTEXT            = 0
glut_ALLOW_DIRECT_CONTEXT              = 1
glut_TRY_DIRECT_CONTEXT                = 2
glut_FORCE_DIRECT_CONTEXT              = 3

-----------------------------------------------------------------------------
-- Behaviour when the user clicks on an "x" to close a window, see
-- glut_ACTION_ON_WINDOW_CLOSE (freeglut extension)

glut_ACTION_EXIT, glut_ACTION_GLUTMAINLOOP_RETURNS,
   glut_ACTION_CONTINUE_EXECUTION :: CInt
glut_ACTION_EXIT                       = 0
glut_ACTION_GLUTMAINLOOP_RETURNS       = 1
glut_ACTION_CONTINUE_EXECUTION         = 2

-----------------------------------------------------------------------------
-- Create a new rendering context when the user opens a new window? See
-- glut_RENDERING_CONTEXT (freeglut extension)

glut_CREATE_NEW_CONTEXT, glut_USE_CURRENT_CONTEXT :: CInt
glut_CREATE_NEW_CONTEXT                = 0
glut_USE_CURRENT_CONTEXT               = 1

-----------------------------------------------------------------------------
-- Process loop function

foreign import CALLCONV unsafe "glutMainLoopEvent"
   glutMainLoopEvent :: IO ()

foreign import CALLCONV unsafe "glutLeaveMainLoop"
   glutLeaveMainLoop :: IO ()

-----------------------------------------------------------------------------
-- Window-specific callback functions
{-
foreign import CALLCONV unsafe "glutMouseWheelFunc"
   void glutMouseWheelFunc( void (* callback)( int, int, int, int ) );

foreign import CALLCONV unsafe "glutCloseFunc"
   void glutCloseFunc( void (* callback)( void ) );

foreign import CALLCONV unsafe "glutWMCloseFunc"
   void glutWMCloseFunc( void (* callback)( void ) );

foreign import CALLCONV unsafe "glutMenuDestroyFunc"
   void glutMenuDestroyFunc( void (* callback)( void ) );
-}
-----------------------------------------------------------------------------
-- State setting and retrieval functions

foreign import CALLCONV unsafe "glutSetOption"
   glutSetOption :: GLenum -> CInt -> IO ()

-----------------------------------------------------------------------------
-- User-data manipulation

foreign import CALLCONV unsafe "glutGetWindowData"
   glutGetWindowData :: IO (Ptr a)

foreign import CALLCONV unsafe "glutSetWindowData"
   glutSetWindowData :: Ptr a -> IO ()

foreign import CALLCONV unsafe "glutGetMenuData"
   glutGetMenuData :: IO (Ptr a)

foreign import CALLCONV unsafe "glutSetMenuData"
   glutSetMenuData :: Ptr a -> IO ()

-----------------------------------------------------------------------------
-- Font stuff

foreign import CALLCONV unsafe "glutBitmapHeight"
   glutBitmapHeight :: Ptr a -> IO CInt

foreign import CALLCONV unsafe "glutStrokeHeight"
   glutStrokeHeight :: Ptr a -> IO GLfloat

foreign import CALLCONV unsafe "glutBitmapString"
   glutBitmapString :: Ptr a -> CString -> IO ()

foreign import CALLCONV unsafe "glutStrokeString"
   glutStrokeString :: Ptr a -> CString -> IO ()

-----------------------------------------------------------------------------
-- Geometry functions

foreign import CALLCONV unsafe "glutWireRhombicDodecahedron"
   glutWireRhombicDodecahedron :: IO ()

foreign import CALLCONV unsafe "glutSolidRhombicDodecahedron"
   glutSolidRhombicDodecahedron :: IO ()

foreign import CALLCONV unsafe "glutWireSierpinskiSponge"
   glutWireSierpinskiSponge :: CInt -> Ptr GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glutSolidSierpinskiSponge"
   glutSolidSierpinskiSponge ::  CInt -> Ptr GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glutWireCylinder"
   glutWireCylinder :: GLdouble -> GLdouble -> GLint -> GLint -> IO ()

foreign import CALLCONV unsafe "glutSolidCylinder"
   glutSolidCylinder :: GLdouble -> GLdouble -> GLint -> GLint -> IO ()

-----------------------------------------------------------------------------
-- Extension functions
foreign import CALLCONV unsafe "glutGetProcAddress"
   glutGetProcAddress :: CString -> IO (FunPtr a)
