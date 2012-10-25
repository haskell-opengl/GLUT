{-# LANGUAGE ForeignFunctionInterface, CPP #-}
-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Raw.Functions
-- Copyright   :  (c) Sven Panne 2009
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- All raw functions from GLUT and freeglut.
--
-----------------------------------------------------------------------------

module Graphics.UI.GLUT.Raw.Functions (
   glutGetProcAddress,
   glutGetColor,
   glutStrokeHeight,
   glutBitmapHeight,
   glutBitmapLength,
   glutBitmapWidth,
   glutCreateMenu,
   glutCreateSubWindow,
   glutCreateWindow,
   glutDeviceGet,
   glutEnterGameMode,
   glutExtensionSupported,
   glutGameModeGet,
   glutGet,
   glutGetMenu,
   glutGetModifiers,
   glutGetWindow,
   glutLayerGet,
   glutStrokeLength,
   glutStrokeWidth,
   glutVideoResizeGet,
   glutGetModeValues,
   glutAddMenuEntry,
   glutAddSubMenu,
   glutAttachMenu,
   glutBitmapCharacter,
   glutBitmapString,
   glutButtonBoxFunc,
   glutChangeToMenuEntry,
   glutChangeToSubMenu,
   glutCloseFunc,
   glutCopyColormap,
   glutDestroyMenu,
   glutDestroyWindow,
   glutDetachMenu,
   glutDialsFunc,
   glutDisplayFunc,
   glutEntryFunc,
   glutEstablishOverlay,
   glutExit,
   glutForceJoystickFunc,
   glutFullScreen,
   glutFullScreenToggle,
   glutGameModeString,
   glutHideOverlay,
   glutHideWindow,
   glutIconifyWindow,
   glutIdleFunc,
   glutIgnoreKeyRepeat,
   glutInit,
   glutInitContextFlags,
   glutInitContextVersion,
   glutInitContextProfile,
   glutInitDisplayMode,
   glutInitDisplayString,
   glutInitWindowPosition,
   glutInitWindowSize,
   glutJoystickFunc,
   glutKeyboardFunc,
   glutKeyboardUpFunc,
   glutLeaveGameMode,
   glutLeaveMainLoop,
   glutMainLoop,
   glutMainLoopEvent,
   glutMenuDestroyFunc,
   glutMenuStateFunc,
   glutMenuStatusFunc,
   glutMotionFunc,
   glutMouseFunc,
   glutMouseWheelFunc,
   glutOverlayDisplayFunc,
   glutPassiveMotionFunc,
   glutPopWindow,
   glutPositionWindow,
   glutPostOverlayRedisplay,
   glutPostRedisplay,
   glutPostWindowOverlayRedisplay,
   glutPostWindowRedisplay,
   glutPushWindow,
   glutRemoveMenuItem,
   glutRemoveOverlay,
   glutReportErrors,
   glutReshapeFunc,
   glutReshapeWindow,
   glutSetColor,
   glutSetCursor,
   glutSetIconTitle,
   glutSetKeyRepeat,
   glutSetMenu,
   glutSetMenuData,
   glutSetOption,
   glutSetWindow,
   glutSetWindowData,
   glutSetWindowTitle,
   glutSetupVideoResizing,
   glutShowOverlay,
   glutShowWindow,
   glutSolidCone,
   glutSolidCube,
   glutSolidCylinder,
   glutSolidDodecahedron,
   glutSolidIcosahedron,
   glutSolidOctahedron,
   glutSolidRhombicDodecahedron,
   glutSolidSierpinskiSponge,
   glutSolidSphere,
   glutSolidTeapot,
   glutSolidTetrahedron,
   glutSolidTorus,
   glutSpaceballButtonFunc,
   glutSpaceballMotionFunc,
   glutSpaceballRotateFunc,
   glutSpecialFunc,
   glutSpecialUpFunc,
   glutStopVideoResizing,
   glutStrokeCharacter,
   glutStrokeString,
   glutSwapBuffers,
   glutTabletButtonFunc,
   glutTabletMotionFunc,
   glutTimerFunc,
   glutUseLayer,
   glutVideoPan,
   glutVideoResize,
   glutVisibilityFunc,
   glutWMCloseFunc,
   glutWarpPointer,
   glutWindowStatusFunc,
   glutWireCone,
   glutWireCube,
   glutWireCylinder,
   glutWireDodecahedron,
   glutWireIcosahedron,
   glutWireOctahedron,
   glutWireRhombicDodecahedron,
   glutWireSierpinskiSponge,
   glutWireSphere,
   glutWireTeapot,
   glutWireTetrahedron,
   glutWireTorus,
   glutGetMenuData,
   glutGetWindowData
) where

import Foreign.C.Types
import Foreign.Ptr
import Graphics.Rendering.OpenGL.Raw.Core31.Types
import Graphics.UI.GLUT.Raw.APIEntry
import Graphics.UI.GLUT.Raw.Callbacks

#include "HsGLUTExt.h"

API_ENTRY(glutGetProcAddress,Ptr CChar -> IO (FunPtr a))
API_ENTRY(glutGetColor,CInt -> CInt -> IO GLfloat)
API_ENTRY(glutStrokeHeight,Ptr a -> IO GLfloat)
API_ENTRY(glutBitmapHeight,Ptr a -> IO CInt)
API_ENTRY(glutBitmapLength,Ptr a -> Ptr CUChar -> IO CInt)
API_ENTRY(glutBitmapWidth,Ptr a -> CInt -> IO CInt)
API_ENTRY(glutCreateMenu,FunPtr MenuFunc -> IO CInt)
API_ENTRY(glutCreateSubWindow,CInt -> CInt -> CInt -> CInt -> CInt -> IO CInt)
API_ENTRY(glutCreateWindow,Ptr CChar -> IO CInt)
API_ENTRY(glutDeviceGet,GLenum -> IO CInt)
API_ENTRY(glutEnterGameMode,IO CInt)
API_ENTRY(glutExtensionSupported,Ptr CChar -> IO CInt)
API_ENTRY(glutGameModeGet,GLenum -> IO CInt)
API_ENTRY(glutGet,GLenum -> IO CInt)
API_ENTRY(glutGetMenu,IO CInt)
API_ENTRY(glutGetModifiers,IO CInt)
API_ENTRY(glutGetWindow,IO CInt)
API_ENTRY(glutLayerGet,GLenum -> IO CInt)
API_ENTRY(glutStrokeLength,Ptr a -> Ptr CUChar -> IO CInt)
API_ENTRY(glutStrokeWidth,Ptr a -> CInt -> IO CInt)
API_ENTRY(glutVideoResizeGet,GLenum -> IO CInt)
API_ENTRY(glutGetModeValues,GLenum -> Ptr CInt -> IO (Ptr CInt))
API_ENTRY(glutAddMenuEntry,Ptr CChar -> CInt -> IO ())
API_ENTRY(glutAddSubMenu,Ptr CChar -> CInt -> IO ())
API_ENTRY(glutAttachMenu,CInt -> IO ())
API_ENTRY(glutBitmapCharacter,Ptr a -> CInt -> IO ())
API_ENTRY(glutBitmapString,Ptr a -> Ptr CUChar -> IO ())
API_ENTRY(glutButtonBoxFunc,FunPtr ButtonBoxFunc -> IO ())
API_ENTRY(glutChangeToMenuEntry,CInt -> Ptr CChar -> CInt -> IO ())
API_ENTRY(glutChangeToSubMenu,CInt -> Ptr CChar -> CInt -> IO ())
API_ENTRY(glutCloseFunc,FunPtr CloseFunc -> IO ())
API_ENTRY(glutCopyColormap,CInt -> IO ())
API_ENTRY(glutDestroyMenu,CInt -> IO ())
API_ENTRY(glutDestroyWindow,CInt -> IO ())
API_ENTRY(glutDetachMenu,CInt -> IO ())
API_ENTRY(glutDialsFunc,FunPtr DialsFunc -> IO ())
API_ENTRY(glutDisplayFunc,FunPtr DisplayFunc -> IO ())
API_ENTRY(glutEntryFunc,FunPtr EntryFunc -> IO ())
API_ENTRY_SAFE(glutEstablishOverlay,IO ())
API_ENTRY(glutExit,IO ())
API_ENTRY(glutForceJoystickFunc,IO ())
API_ENTRY(glutFullScreen,IO ())
API_ENTRY(glutFullScreenToggle,IO ())
API_ENTRY(glutGameModeString,Ptr CChar -> IO ())
API_ENTRY_SAFE(glutHideOverlay,IO ())
API_ENTRY(glutHideWindow,IO ())
API_ENTRY(glutIconifyWindow,IO ())
API_ENTRY(glutIdleFunc,FunPtr IdleFunc -> IO ())
API_ENTRY(glutIgnoreKeyRepeat,CInt -> IO ())
API_ENTRY(glutInit,Ptr CInt -> Ptr (Ptr CChar) -> IO ())
API_ENTRY(glutInitContextFlags,CInt -> IO ())
API_ENTRY(glutInitContextVersion,CInt -> CInt -> IO ())
API_ENTRY(glutInitContextProfile,CInt -> IO ())
API_ENTRY(glutInitDisplayMode,CUInt -> IO ())
API_ENTRY(glutInitDisplayString,Ptr CChar -> IO ())
API_ENTRY(glutInitWindowPosition,CInt -> CInt -> IO ())
API_ENTRY(glutInitWindowSize,CInt -> CInt -> IO ())
API_ENTRY(glutJoystickFunc,FunPtr JoystickFunc -> CInt -> IO ())
API_ENTRY(glutKeyboardFunc,FunPtr KeyboardFunc -> IO ())
API_ENTRY(glutKeyboardUpFunc,FunPtr KeyboardUpFunc -> IO ())
API_ENTRY(glutLeaveGameMode,IO ())
API_ENTRY_SAFE(glutLeaveMainLoop,IO ())
API_ENTRY_SAFE(glutMainLoop,IO ())
API_ENTRY_SAFE(glutMainLoopEvent,IO ())
API_ENTRY(glutMenuDestroyFunc,FunPtr MenuDestroyFunc -> IO ())
API_ENTRY(glutMenuStateFunc,FunPtr MenuStateFunc -> IO ())
API_ENTRY(glutMenuStatusFunc,FunPtr MenuStatusFunc -> IO ())
API_ENTRY(glutMotionFunc,FunPtr MotionFunc -> IO ())
API_ENTRY(glutMouseFunc,FunPtr MouseFunc -> IO ())
API_ENTRY(glutMouseWheelFunc,FunPtr MouseWheelFunc -> IO ())
API_ENTRY(glutOverlayDisplayFunc,FunPtr OverlayDisplayFunc -> IO ())
API_ENTRY(glutPassiveMotionFunc,FunPtr PassiveMotionFunc -> IO ())
API_ENTRY(glutPopWindow,IO ())
API_ENTRY(glutPositionWindow,CInt -> CInt -> IO ())
API_ENTRY_SAFE(glutPostOverlayRedisplay,IO ())
API_ENTRY(glutPostRedisplay,IO ())
API_ENTRY_SAFE(glutPostWindowOverlayRedisplay,CInt -> IO ())
API_ENTRY(glutPostWindowRedisplay,CInt -> IO ())
API_ENTRY(glutPushWindow,IO ())
API_ENTRY(glutRemoveMenuItem,CInt -> IO ())
API_ENTRY_SAFE(glutRemoveOverlay,IO ())
API_ENTRY(glutReportErrors,IO ())
API_ENTRY(glutReshapeFunc,FunPtr ReshapeFunc -> IO ())
API_ENTRY(glutReshapeWindow,CInt -> CInt -> IO ())
API_ENTRY(glutSetColor,CInt -> GLfloat -> GLfloat -> GLfloat -> IO ())
API_ENTRY(glutSetCursor,CInt -> IO ())
API_ENTRY(glutSetIconTitle,Ptr CChar -> IO ())
API_ENTRY(glutSetKeyRepeat,CInt -> IO ())
API_ENTRY(glutSetMenu,CInt -> IO ())
API_ENTRY(glutSetMenuData,Ptr a -> IO ())
API_ENTRY(glutSetOption,GLenum -> CInt -> IO ())
API_ENTRY(glutSetWindow,CInt -> IO ())
API_ENTRY(glutSetWindowData,Ptr a -> IO ())
API_ENTRY(glutSetWindowTitle,Ptr CChar -> IO ())
API_ENTRY(glutSetupVideoResizing,IO ())
API_ENTRY_SAFE(glutShowOverlay,IO ())
API_ENTRY(glutShowWindow,IO ())
API_ENTRY(glutSolidCone,GLdouble -> GLdouble -> GLint -> GLint -> IO ())
API_ENTRY(glutSolidCube,GLdouble -> IO ())
API_ENTRY(glutSolidCylinder,GLdouble -> GLdouble -> GLint -> GLint -> IO ())
API_ENTRY(glutSolidDodecahedron,IO ())
API_ENTRY(glutSolidIcosahedron,IO ())
API_ENTRY(glutSolidOctahedron,IO ())
API_ENTRY(glutSolidRhombicDodecahedron,IO ())
API_ENTRY(glutSolidSierpinskiSponge,CInt -> Ptr GLdouble -> GLdouble -> IO ())
API_ENTRY(glutSolidSphere,GLdouble -> GLint -> GLint -> IO ())
API_ENTRY(glutSolidTeapot,GLdouble -> IO ())
API_ENTRY(glutSolidTetrahedron,IO ())
API_ENTRY(glutSolidTorus,GLdouble -> GLdouble -> GLint -> GLint -> IO ())
API_ENTRY(glutSpaceballButtonFunc,FunPtr SpaceballButtonFunc -> IO ())
API_ENTRY(glutSpaceballMotionFunc,FunPtr SpaceballMotionFunc -> IO ())
API_ENTRY(glutSpaceballRotateFunc,FunPtr SpaceballRotateFunc -> IO ())
API_ENTRY(glutSpecialFunc,FunPtr SpecialFunc -> IO ())
API_ENTRY(glutSpecialUpFunc,FunPtr SpecialUpFunc -> IO ())
API_ENTRY(glutStopVideoResizing,IO ())
API_ENTRY(glutStrokeCharacter,Ptr a -> CInt -> IO ())
API_ENTRY(glutStrokeString,Ptr a -> Ptr CUChar -> IO ())
API_ENTRY(glutSwapBuffers,IO ())
API_ENTRY(glutTabletButtonFunc,FunPtr TabletButtonFunc -> IO ())
API_ENTRY(glutTabletMotionFunc,FunPtr TabletMotionFunc -> IO ())
API_ENTRY(glutTimerFunc,CUInt -> FunPtr TimerFunc -> CInt -> IO ())
API_ENTRY_SAFE(glutUseLayer,GLenum -> IO ())
API_ENTRY(glutVideoPan,CInt -> CInt -> CInt -> CInt -> IO ())
API_ENTRY(glutVideoResize,CInt -> CInt -> CInt -> CInt -> IO ())
API_ENTRY(glutVisibilityFunc,FunPtr VisibilityFunc -> IO ())
API_ENTRY(glutWMCloseFunc,FunPtr WMCloseFunc -> IO ())
API_ENTRY(glutWarpPointer,CInt -> CInt -> IO ())
API_ENTRY(glutWindowStatusFunc,FunPtr WindowStatusFunc -> IO ())
API_ENTRY(glutWireCone,GLdouble -> GLdouble -> GLint -> GLint -> IO ())
API_ENTRY(glutWireCube,GLdouble -> IO ())
API_ENTRY(glutWireCylinder,GLdouble -> GLdouble -> GLint -> GLint -> IO ())
API_ENTRY(glutWireDodecahedron,IO ())
API_ENTRY(glutWireIcosahedron,IO ())
API_ENTRY(glutWireOctahedron,IO ())
API_ENTRY(glutWireRhombicDodecahedron,IO ())
API_ENTRY(glutWireSierpinskiSponge,CInt -> Ptr GLdouble -> GLdouble -> IO ())
API_ENTRY(glutWireSphere,GLdouble -> GLint -> GLint -> IO ())
API_ENTRY(glutWireTeapot,GLdouble -> IO ())
API_ENTRY(glutWireTetrahedron,IO ())
API_ENTRY(glutWireTorus,GLdouble -> GLdouble -> GLint -> GLint -> IO ())
API_ENTRY(glutGetMenuData,IO (Ptr a))
API_ENTRY(glutGetWindowData,IO (Ptr a))
