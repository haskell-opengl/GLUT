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
