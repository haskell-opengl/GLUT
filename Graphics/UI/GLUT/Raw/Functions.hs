{-# LANGUAGE ForeignFunctionInterface, CPP #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Raw.Functions
-- Copyright   :  (c) Sven Panne 2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- All raw functions from GLUT and freeglut.
--
-----------------------------------------------------------------------------

module Graphics.UI.GLUT.Raw.Functions (
   glutAddMenuEntry,
   glutAddSubMenu,
   glutAppStatusFunc,
   glutAttachMenu,
   glutBitmapCharacter,
   glutBitmapHeight,
   glutBitmapLength,
   glutBitmapString,
   glutBitmapWidth,
   glutButtonBoxFunc,
   glutChangeToMenuEntry,
   glutChangeToSubMenu,
   glutCloseFunc,
   glutCopyColormap,
   glutCreateMenu,
   glutCreateSubWindow,
   glutCreateWindow,
   glutDestroyMenu,
   glutDestroyWindow,
   glutDetachMenu,
   glutDeviceGet,
   glutDialsFunc,
   glutDisplayFunc,
   glutEnterGameMode,
   glutEntryFunc,
   glutEstablishOverlay,
   glutExit,
   glutExtensionSupported,
   glutForceJoystickFunc,
   glutFullScreen,
   glutFullScreenToggle,
   glutGameModeGet,
   glutGameModeString,
   glutGet,
   glutGetColor,
   glutGetMenu,
   glutGetMenuData,
   glutGetModeValues,
   glutGetModifiers,
   glutGetProcAddress,
   glutGetWindow,
   glutGetWindowData,
   glutHideOverlay,
   glutHideWindow,
   glutIconifyWindow,
   glutIdleFunc,
   glutIgnoreKeyRepeat,
   glutInit,
   glutInitContextFlags,
   glutInitContextFunc,
   glutInitContextProfile,
   glutInitContextVersion,
   glutInitDisplayMode,
   glutInitDisplayString,
   glutInitWindowPosition,
   glutInitWindowSize,
   glutJoystickFunc,
   glutKeyboardFunc,
   glutKeyboardUpFunc,
   glutLayerGet,
   glutLeaveFullScreen,
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
   glutMultiButtonFunc,
   glutMultiEntryFunc,
   glutMultiMotionFunc,
   glutMultiPassiveFunc,
   glutOverlayDisplayFunc,
   glutPassiveMotionFunc,
   glutPopWindow,
   glutPositionFunc,
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
   glutSetMenuFont,
   glutSetOption,
   glutSetVertexAttribCoord3,
   glutSetVertexAttribNormal,
   glutSetVertexAttribTexCoord2,
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
   glutSolidTeacup,
   glutSolidTeapot,
   glutSolidTeaspoon,
   glutSolidTetrahedron,
   glutSolidTorus,
   glutSpaceballButtonFunc,
   glutSpaceballMotionFunc,
   glutSpaceballRotateFunc,
   glutSpecialFunc,
   glutSpecialUpFunc,
   glutStopVideoResizing,
   glutStrokeCharacter,
   glutStrokeHeight,
   glutStrokeLength,
   glutStrokeString,
   glutStrokeWidth,
   glutSwapBuffers,
   glutTabletButtonFunc,
   glutTabletMotionFunc,
   glutTimerFunc,
   glutUseLayer,
   glutVideoPan,
   glutVideoResize,
   glutVideoResizeGet,
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
   glutWireTeacup,
   glutWireTeapot,
   glutWireTeaspoon,
   glutWireTetrahedron,
   glutWireTorus
) where

import Foreign.C.Types
import Foreign.Ptr
import Graphics.Rendering.OpenGL.Raw.Core31.Types
import Graphics.UI.GLUT.Raw.APIEntry
import Graphics.UI.GLUT.Raw.Callbacks

#include "HsGLUTExt.h"

API_ENTRY(dyn_glutAddMenuEntry,ptr_glutAddMenuEntry,"glutAddMenuEntry",glutAddMenuEntry,Ptr CChar -> CInt -> IO ())
API_ENTRY(dyn_glutAddSubMenu,ptr_glutAddSubMenu,"glutAddSubMenu",glutAddSubMenu,Ptr CChar -> CInt -> IO ())
API_ENTRY(dyn_glutAppStatusFunc,ptr_glutAppStatusFunc,"glutAppStatusFunc",glutAppStatusFunc,FunPtr AppStatusFunc -> IO ())
API_ENTRY(dyn_glutAttachMenu,ptr_glutAttachMenu,"glutAttachMenu",glutAttachMenu,CInt -> IO ())
API_ENTRY(dyn_glutBitmapCharacter,ptr_glutBitmapCharacter,"glutBitmapCharacter",glutBitmapCharacter,Ptr a -> CInt -> IO ())
API_ENTRY(dyn_glutBitmapHeight,ptr_glutBitmapHeight,"glutBitmapHeight",glutBitmapHeight,Ptr a -> IO CInt)
API_ENTRY(dyn_glutBitmapLength,ptr_glutBitmapLength,"glutBitmapLength",glutBitmapLength,Ptr a -> Ptr CUChar -> IO CInt)
API_ENTRY(dyn_glutBitmapString,ptr_glutBitmapString,"glutBitmapString",glutBitmapString,Ptr a -> Ptr CUChar -> IO ())
API_ENTRY(dyn_glutBitmapWidth,ptr_glutBitmapWidth,"glutBitmapWidth",glutBitmapWidth,Ptr a -> CInt -> IO CInt)
API_ENTRY(dyn_glutButtonBoxFunc,ptr_glutButtonBoxFunc,"glutButtonBoxFunc",glutButtonBoxFunc,FunPtr ButtonBoxFunc -> IO ())
API_ENTRY(dyn_glutChangeToMenuEntry,ptr_glutChangeToMenuEntry,"glutChangeToMenuEntry",glutChangeToMenuEntry,CInt -> Ptr CChar -> CInt -> IO ())
API_ENTRY(dyn_glutChangeToSubMenu,ptr_glutChangeToSubMenu,"glutChangeToSubMenu",glutChangeToSubMenu,CInt -> Ptr CChar -> CInt -> IO ())
API_ENTRY(dyn_glutCloseFunc,ptr_glutCloseFunc,"glutCloseFunc",glutCloseFunc,FunPtr CloseFunc -> IO ())
API_ENTRY(dyn_glutCopyColormap,ptr_glutCopyColormap,"glutCopyColormap",glutCopyColormap,CInt -> IO ())
API_ENTRY(dyn_glutCreateMenu,ptr_glutCreateMenu,"glutCreateMenu",glutCreateMenu,FunPtr MenuFunc -> IO CInt)
API_ENTRY(dyn_glutCreateSubWindow,ptr_glutCreateSubWindow,"glutCreateSubWindow",glutCreateSubWindow,CInt -> CInt -> CInt -> CInt -> CInt -> IO CInt)
API_ENTRY(dyn_glutCreateWindow,ptr_glutCreateWindow,"glutCreateWindow",glutCreateWindow,Ptr CChar -> IO CInt)
API_ENTRY(dyn_glutDestroyMenu,ptr_glutDestroyMenu,"glutDestroyMenu",glutDestroyMenu,CInt -> IO ())
API_ENTRY(dyn_glutDestroyWindow,ptr_glutDestroyWindow,"glutDestroyWindow",glutDestroyWindow,CInt -> IO ())
API_ENTRY(dyn_glutDetachMenu,ptr_glutDetachMenu,"glutDetachMenu",glutDetachMenu,CInt -> IO ())
API_ENTRY(dyn_glutDeviceGet,ptr_glutDeviceGet,"glutDeviceGet",glutDeviceGet,GLenum -> IO CInt)
API_ENTRY(dyn_glutDialsFunc,ptr_glutDialsFunc,"glutDialsFunc",glutDialsFunc,FunPtr DialsFunc -> IO ())
API_ENTRY(dyn_glutDisplayFunc,ptr_glutDisplayFunc,"glutDisplayFunc",glutDisplayFunc,FunPtr DisplayFunc -> IO ())
API_ENTRY(dyn_glutEnterGameMode,ptr_glutEnterGameMode,"glutEnterGameMode",glutEnterGameMode,IO CInt)
API_ENTRY(dyn_glutEntryFunc,ptr_glutEntryFunc,"glutEntryFunc",glutEntryFunc,FunPtr EntryFunc -> IO ())
API_ENTRY_SAFE(dyn_glutEstablishOverlay,ptr_glutEstablishOverlay,"glutEstablishOverlay",glutEstablishOverlay,IO ())
API_ENTRY(dyn_glutExit,ptr_glutExit,"glutExit",glutExit,IO ())
API_ENTRY(dyn_glutExtensionSupported,ptr_glutExtensionSupported,"glutExtensionSupported",glutExtensionSupported,Ptr CChar -> IO CInt)
API_ENTRY(dyn_glutForceJoystickFunc,ptr_glutForceJoystickFunc,"glutForceJoystickFunc",glutForceJoystickFunc,IO ())
API_ENTRY(dyn_glutFullScreen,ptr_glutFullScreen,"glutFullScreen",glutFullScreen,IO ())
API_ENTRY(dyn_glutFullScreenToggle,ptr_glutFullScreenToggle,"glutFullScreenToggle",glutFullScreenToggle,IO ())
API_ENTRY(dyn_glutGameModeGet,ptr_glutGameModeGet,"glutGameModeGet",glutGameModeGet,GLenum -> IO CInt)
API_ENTRY(dyn_glutGameModeString,ptr_glutGameModeString,"glutGameModeString",glutGameModeString,Ptr CChar -> IO ())
API_ENTRY(dyn_glutGet,ptr_glutGet,"glutGet",glutGet,GLenum -> IO CInt)
API_ENTRY(dyn_glutGetColor,ptr_glutGetColor,"glutGetColor",glutGetColor,CInt -> CInt -> IO GLfloat)
API_ENTRY(dyn_glutGetMenu,ptr_glutGetMenu,"glutGetMenu",glutGetMenu,IO CInt)
API_ENTRY(dyn_glutGetMenuData,ptr_glutGetMenuData,"glutGetMenuData",glutGetMenuData,IO (Ptr a))
API_ENTRY(dyn_glutGetModeValues,ptr_glutGetModeValues,"glutGetModeValues",glutGetModeValues,GLenum -> Ptr CInt -> IO (Ptr CInt))
API_ENTRY(dyn_glutGetModifiers,ptr_glutGetModifiers,"glutGetModifiers",glutGetModifiers,IO CInt)
API_ENTRY(dyn_glutGetProcAddress,ptr_glutGetProcAddress,"glutGetProcAddress",glutGetProcAddress,Ptr CChar -> IO (FunPtr a))
API_ENTRY(dyn_glutGetWindow,ptr_glutGetWindow,"glutGetWindow",glutGetWindow,IO CInt)
API_ENTRY(dyn_glutGetWindowData,ptr_glutGetWindowData,"glutGetWindowData",glutGetWindowData,IO (Ptr a))
API_ENTRY_SAFE(dyn_glutHideOverlay,ptr_glutHideOverlay,"glutHideOverlay",glutHideOverlay,IO ())
API_ENTRY(dyn_glutHideWindow,ptr_glutHideWindow,"glutHideWindow",glutHideWindow,IO ())
API_ENTRY(dyn_glutIconifyWindow,ptr_glutIconifyWindow,"glutIconifyWindow",glutIconifyWindow,IO ())
API_ENTRY(dyn_glutIdleFunc,ptr_glutIdleFunc,"glutIdleFunc",glutIdleFunc,FunPtr IdleFunc -> IO ())
API_ENTRY(dyn_glutIgnoreKeyRepeat,ptr_glutIgnoreKeyRepeat,"glutIgnoreKeyRepeat",glutIgnoreKeyRepeat,CInt -> IO ())
API_ENTRY(dyn_glutInit,ptr_glutInit,"glutInit",glutInit,Ptr CInt -> Ptr (Ptr CChar) -> IO ())
API_ENTRY(dyn_glutInitContextFlags,ptr_glutInitContextFlags,"glutInitContextFlags",glutInitContextFlags,CInt -> IO ())
API_ENTRY(dyn_glutInitContextFunc,ptr_glutInitContextFunc,"glutInitContextFunc",glutInitContextFunc,FunPtr InitContextFunc -> IO ())
API_ENTRY(dyn_glutInitContextProfile,ptr_glutInitContextProfile,"glutInitContextProfile",glutInitContextProfile,CInt -> IO ())
API_ENTRY(dyn_glutInitContextVersion,ptr_glutInitContextVersion,"glutInitContextVersion",glutInitContextVersion,CInt -> CInt -> IO ())
API_ENTRY(dyn_glutInitDisplayMode,ptr_glutInitDisplayMode,"glutInitDisplayMode",glutInitDisplayMode,CUInt -> IO ())
API_ENTRY(dyn_glutInitDisplayString,ptr_glutInitDisplayString,"glutInitDisplayString",glutInitDisplayString,Ptr CChar -> IO ())
API_ENTRY(dyn_glutInitWindowPosition,ptr_glutInitWindowPosition,"glutInitWindowPosition",glutInitWindowPosition,CInt -> CInt -> IO ())
API_ENTRY(dyn_glutInitWindowSize,ptr_glutInitWindowSize,"glutInitWindowSize",glutInitWindowSize,CInt -> CInt -> IO ())
API_ENTRY(dyn_glutJoystickFunc,ptr_glutJoystickFunc,"glutJoystickFunc",glutJoystickFunc,FunPtr JoystickFunc -> CInt -> IO ())
API_ENTRY(dyn_glutKeyboardFunc,ptr_glutKeyboardFunc,"glutKeyboardFunc",glutKeyboardFunc,FunPtr KeyboardFunc -> IO ())
API_ENTRY(dyn_glutKeyboardUpFunc,ptr_glutKeyboardUpFunc,"glutKeyboardUpFunc",glutKeyboardUpFunc,FunPtr KeyboardUpFunc -> IO ())
API_ENTRY(dyn_glutLayerGet,ptr_glutLayerGet,"glutLayerGet",glutLayerGet,GLenum -> IO CInt)
API_ENTRY(dyn_glutLeaveFullScreen,ptr_glutLeaveFullScreen,"glutLeaveFullScreen",glutLeaveFullScreen,IO ())
API_ENTRY(dyn_glutLeaveGameMode,ptr_glutLeaveGameMode,"glutLeaveGameMode",glutLeaveGameMode,IO ())
API_ENTRY_SAFE(dyn_glutLeaveMainLoop,ptr_glutLeaveMainLoop,"glutLeaveMainLoop",glutLeaveMainLoop,IO ())
API_ENTRY_SAFE(dyn_glutMainLoop,ptr_glutMainLoop,"glutMainLoop",glutMainLoop,IO ())
API_ENTRY_SAFE(dyn_glutMainLoopEvent,ptr_glutMainLoopEvent,"glutMainLoopEvent",glutMainLoopEvent,IO ())
API_ENTRY(dyn_glutMenuDestroyFunc,ptr_glutMenuDestroyFunc,"glutMenuDestroyFunc",glutMenuDestroyFunc,FunPtr MenuDestroyFunc -> IO ())
API_ENTRY(dyn_glutMenuStateFunc,ptr_glutMenuStateFunc,"glutMenuStateFunc",glutMenuStateFunc,FunPtr MenuStateFunc -> IO ())
API_ENTRY(dyn_glutMenuStatusFunc,ptr_glutMenuStatusFunc,"glutMenuStatusFunc",glutMenuStatusFunc,FunPtr MenuStatusFunc -> IO ())
API_ENTRY(dyn_glutMotionFunc,ptr_glutMotionFunc,"glutMotionFunc",glutMotionFunc,FunPtr MotionFunc -> IO ())
API_ENTRY(dyn_glutMouseFunc,ptr_glutMouseFunc,"glutMouseFunc",glutMouseFunc,FunPtr MouseFunc -> IO ())
API_ENTRY(dyn_glutMouseWheelFunc,ptr_glutMouseWheelFunc,"glutMouseWheelFunc",glutMouseWheelFunc,FunPtr MouseWheelFunc -> IO ())
API_ENTRY(dyn_glutMultiButtonFunc,ptr_glutMultiButtonFunc,"glutMultiButtonFunc",glutMultiButtonFunc,FunPtr MultiButtonFunc -> IO ())
API_ENTRY(dyn_glutMultiEntryFunc,ptr_glutMultiEntryFunc,"glutMultiEntryFunc",glutMultiEntryFunc,FunPtr MultiEntryFunc -> IO ())
API_ENTRY(dyn_glutMultiMotionFunc,ptr_glutMultiMotionFunc,"glutMultiMotionFunc",glutMultiMotionFunc,FunPtr MultiMotionFunc -> IO ())
API_ENTRY(dyn_glutMultiPassiveFunc,ptr_glutMultiPassiveFunc,"glutMultiPassiveFunc",glutMultiPassiveFunc,FunPtr MultiPassiveFunc -> IO ())
API_ENTRY(dyn_glutOverlayDisplayFunc,ptr_glutOverlayDisplayFunc,"glutOverlayDisplayFunc",glutOverlayDisplayFunc,FunPtr OverlayDisplayFunc -> IO ())
API_ENTRY(dyn_glutPassiveMotionFunc,ptr_glutPassiveMotionFunc,"glutPassiveMotionFunc",glutPassiveMotionFunc,FunPtr PassiveMotionFunc -> IO ())
API_ENTRY(dyn_glutPopWindow,ptr_glutPopWindow,"glutPopWindow",glutPopWindow,IO ())
API_ENTRY(dyn_glutPositionFunc,ptr_glutPositionFunc,"glutPositionFunc",glutPositionFunc,FunPtr PositionFunc -> IO ())
API_ENTRY(dyn_glutPositionWindow,ptr_glutPositionWindow,"glutPositionWindow",glutPositionWindow,CInt -> CInt -> IO ())
API_ENTRY_SAFE(dyn_glutPostOverlayRedisplay,ptr_glutPostOverlayRedisplay,"glutPostOverlayRedisplay",glutPostOverlayRedisplay,IO ())
API_ENTRY(dyn_glutPostRedisplay,ptr_glutPostRedisplay,"glutPostRedisplay",glutPostRedisplay,IO ())
API_ENTRY_SAFE(dyn_glutPostWindowOverlayRedisplay,ptr_glutPostWindowOverlayRedisplay,"glutPostWindowOverlayRedisplay",glutPostWindowOverlayRedisplay,CInt -> IO ())
API_ENTRY(dyn_glutPostWindowRedisplay,ptr_glutPostWindowRedisplay,"glutPostWindowRedisplay",glutPostWindowRedisplay,CInt -> IO ())
API_ENTRY(dyn_glutPushWindow,ptr_glutPushWindow,"glutPushWindow",glutPushWindow,IO ())
API_ENTRY(dyn_glutRemoveMenuItem,ptr_glutRemoveMenuItem,"glutRemoveMenuItem",glutRemoveMenuItem,CInt -> IO ())
API_ENTRY_SAFE(dyn_glutRemoveOverlay,ptr_glutRemoveOverlay,"glutRemoveOverlay",glutRemoveOverlay,IO ())
API_ENTRY(dyn_glutReportErrors,ptr_glutReportErrors,"glutReportErrors",glutReportErrors,IO ())
API_ENTRY(dyn_glutReshapeFunc,ptr_glutReshapeFunc,"glutReshapeFunc",glutReshapeFunc,FunPtr ReshapeFunc -> IO ())
API_ENTRY(dyn_glutReshapeWindow,ptr_glutReshapeWindow,"glutReshapeWindow",glutReshapeWindow,CInt -> CInt -> IO ())
API_ENTRY(dyn_glutSetColor,ptr_glutSetColor,"glutSetColor",glutSetColor,CInt -> GLfloat -> GLfloat -> GLfloat -> IO ())
API_ENTRY(dyn_glutSetCursor,ptr_glutSetCursor,"glutSetCursor",glutSetCursor,CInt -> IO ())
API_ENTRY(dyn_glutSetIconTitle,ptr_glutSetIconTitle,"glutSetIconTitle",glutSetIconTitle,Ptr CChar -> IO ())
API_ENTRY(dyn_glutSetKeyRepeat,ptr_glutSetKeyRepeat,"glutSetKeyRepeat",glutSetKeyRepeat,CInt -> IO ())
API_ENTRY(dyn_glutSetMenu,ptr_glutSetMenu,"glutSetMenu",glutSetMenu,CInt -> IO ())
API_ENTRY(dyn_glutSetMenuData,ptr_glutSetMenuData,"glutSetMenuData",glutSetMenuData,Ptr a -> IO ())
API_ENTRY(dyn_glutSetMenuFont,ptr_glutSetMenuFont,"glutSetMenuFont",glutSetMenuFont,GLint -> Ptr a -> IO ())
API_ENTRY(dyn_glutSetOption,ptr_glutSetOption,"glutSetOption",glutSetOption,GLenum -> CInt -> IO ())
API_ENTRY(dyn_glutSetVertexAttribCoord3,ptr_glutSetVertexAttribCoord3,"glutSetVertexAttribCoord3",glutSetVertexAttribCoord3,GLint -> IO ())
API_ENTRY(dyn_glutSetVertexAttribNormal,ptr_glutSetVertexAttribNormal,"glutSetVertexAttribNormal",glutSetVertexAttribNormal,GLint -> IO ())
API_ENTRY(dyn_glutSetVertexAttribTexCoord2,ptr_glutSetVertexAttribTexCoord2,"glutSetVertexAttribTexCoord2",glutSetVertexAttribTexCoord2,GLint -> IO ())
API_ENTRY(dyn_glutSetWindow,ptr_glutSetWindow,"glutSetWindow",glutSetWindow,CInt -> IO ())
API_ENTRY(dyn_glutSetWindowData,ptr_glutSetWindowData,"glutSetWindowData",glutSetWindowData,Ptr a -> IO ())
API_ENTRY(dyn_glutSetWindowTitle,ptr_glutSetWindowTitle,"glutSetWindowTitle",glutSetWindowTitle,Ptr CChar -> IO ())
API_ENTRY(dyn_glutSetupVideoResizing,ptr_glutSetupVideoResizing,"glutSetupVideoResizing",glutSetupVideoResizing,IO ())
API_ENTRY_SAFE(dyn_glutShowOverlay,ptr_glutShowOverlay,"glutShowOverlay",glutShowOverlay,IO ())
API_ENTRY(dyn_glutShowWindow,ptr_glutShowWindow,"glutShowWindow",glutShowWindow,IO ())
API_ENTRY(dyn_glutSolidCone,ptr_glutSolidCone,"glutSolidCone",glutSolidCone,GLdouble -> GLdouble -> GLint -> GLint -> IO ())
API_ENTRY(dyn_glutSolidCube,ptr_glutSolidCube,"glutSolidCube",glutSolidCube,GLdouble -> IO ())
API_ENTRY(dyn_glutSolidCylinder,ptr_glutSolidCylinder,"glutSolidCylinder",glutSolidCylinder,GLdouble -> GLdouble -> GLint -> GLint -> IO ())
API_ENTRY(dyn_glutSolidDodecahedron,ptr_glutSolidDodecahedron,"glutSolidDodecahedron",glutSolidDodecahedron,IO ())
API_ENTRY(dyn_glutSolidIcosahedron,ptr_glutSolidIcosahedron,"glutSolidIcosahedron",glutSolidIcosahedron,IO ())
API_ENTRY(dyn_glutSolidOctahedron,ptr_glutSolidOctahedron,"glutSolidOctahedron",glutSolidOctahedron,IO ())
API_ENTRY(dyn_glutSolidRhombicDodecahedron,ptr_glutSolidRhombicDodecahedron,"glutSolidRhombicDodecahedron",glutSolidRhombicDodecahedron,IO ())
API_ENTRY(dyn_glutSolidSierpinskiSponge,ptr_glutSolidSierpinskiSponge,"glutSolidSierpinskiSponge",glutSolidSierpinskiSponge,CInt -> Ptr GLdouble -> GLdouble -> IO ())
API_ENTRY(dyn_glutSolidSphere,ptr_glutSolidSphere,"glutSolidSphere",glutSolidSphere,GLdouble -> GLint -> GLint -> IO ())
API_ENTRY(dyn_glutSolidTeacup,ptr_glutSolidTeacup,"glutSolidTeacup",glutSolidTeacup,GLdouble -> IO ())
API_ENTRY(dyn_glutSolidTeapot,ptr_glutSolidTeapot,"glutSolidTeapot",glutSolidTeapot,GLdouble -> IO ())
API_ENTRY(dyn_glutSolidTeaspoon,ptr_glutSolidTeaspoon,"glutSolidTeaspoon",glutSolidTeaspoon,GLdouble -> IO ())
API_ENTRY(dyn_glutSolidTetrahedron,ptr_glutSolidTetrahedron,"glutSolidTetrahedron",glutSolidTetrahedron,IO ())
API_ENTRY(dyn_glutSolidTorus,ptr_glutSolidTorus,"glutSolidTorus",glutSolidTorus,GLdouble -> GLdouble -> GLint -> GLint -> IO ())
API_ENTRY(dyn_glutSpaceballButtonFunc,ptr_glutSpaceballButtonFunc,"glutSpaceballButtonFunc",glutSpaceballButtonFunc,FunPtr SpaceballButtonFunc -> IO ())
API_ENTRY(dyn_glutSpaceballMotionFunc,ptr_glutSpaceballMotionFunc,"glutSpaceballMotionFunc",glutSpaceballMotionFunc,FunPtr SpaceballMotionFunc -> IO ())
API_ENTRY(dyn_glutSpaceballRotateFunc,ptr_glutSpaceballRotateFunc,"glutSpaceballRotateFunc",glutSpaceballRotateFunc,FunPtr SpaceballRotateFunc -> IO ())
API_ENTRY(dyn_glutSpecialFunc,ptr_glutSpecialFunc,"glutSpecialFunc",glutSpecialFunc,FunPtr SpecialFunc -> IO ())
API_ENTRY(dyn_glutSpecialUpFunc,ptr_glutSpecialUpFunc,"glutSpecialUpFunc",glutSpecialUpFunc,FunPtr SpecialUpFunc -> IO ())
API_ENTRY(dyn_glutStopVideoResizing,ptr_glutStopVideoResizing,"glutStopVideoResizing",glutStopVideoResizing,IO ())
API_ENTRY(dyn_glutStrokeCharacter,ptr_glutStrokeCharacter,"glutStrokeCharacter",glutStrokeCharacter,Ptr a -> CInt -> IO ())
API_ENTRY(dyn_glutStrokeHeight,ptr_glutStrokeHeight,"glutStrokeHeight",glutStrokeHeight,Ptr a -> IO GLfloat)
API_ENTRY(dyn_glutStrokeLength,ptr_glutStrokeLength,"glutStrokeLength",glutStrokeLength,Ptr a -> Ptr CUChar -> IO CInt)
API_ENTRY(dyn_glutStrokeString,ptr_glutStrokeString,"glutStrokeString",glutStrokeString,Ptr a -> Ptr CUChar -> IO ())
API_ENTRY(dyn_glutStrokeWidth,ptr_glutStrokeWidth,"glutStrokeWidth",glutStrokeWidth,Ptr a -> CInt -> IO CInt)
API_ENTRY(dyn_glutSwapBuffers,ptr_glutSwapBuffers,"glutSwapBuffers",glutSwapBuffers,IO ())
API_ENTRY(dyn_glutTabletButtonFunc,ptr_glutTabletButtonFunc,"glutTabletButtonFunc",glutTabletButtonFunc,FunPtr TabletButtonFunc -> IO ())
API_ENTRY(dyn_glutTabletMotionFunc,ptr_glutTabletMotionFunc,"glutTabletMotionFunc",glutTabletMotionFunc,FunPtr TabletMotionFunc -> IO ())
API_ENTRY(dyn_glutTimerFunc,ptr_glutTimerFunc,"glutTimerFunc",glutTimerFunc,CUInt -> FunPtr TimerFunc -> CInt -> IO ())
API_ENTRY_SAFE(dyn_glutUseLayer,ptr_glutUseLayer,"glutUseLayer",glutUseLayer,GLenum -> IO ())
API_ENTRY(dyn_glutVideoPan,ptr_glutVideoPan,"glutVideoPan",glutVideoPan,CInt -> CInt -> CInt -> CInt -> IO ())
API_ENTRY(dyn_glutVideoResize,ptr_glutVideoResize,"glutVideoResize",glutVideoResize,CInt -> CInt -> CInt -> CInt -> IO ())
API_ENTRY(dyn_glutVideoResizeGet,ptr_glutVideoResizeGet,"glutVideoResizeGet",glutVideoResizeGet,GLenum -> IO CInt)
API_ENTRY(dyn_glutVisibilityFunc,ptr_glutVisibilityFunc,"glutVisibilityFunc",glutVisibilityFunc,FunPtr VisibilityFunc -> IO ())
API_ENTRY(dyn_glutWMCloseFunc,ptr_glutWMCloseFunc,"glutWMCloseFunc",glutWMCloseFunc,FunPtr WMCloseFunc -> IO ())
API_ENTRY(dyn_glutWarpPointer,ptr_glutWarpPointer,"glutWarpPointer",glutWarpPointer,CInt -> CInt -> IO ())
API_ENTRY(dyn_glutWindowStatusFunc,ptr_glutWindowStatusFunc,"glutWindowStatusFunc",glutWindowStatusFunc,FunPtr WindowStatusFunc -> IO ())
API_ENTRY(dyn_glutWireCone,ptr_glutWireCone,"glutWireCone",glutWireCone,GLdouble -> GLdouble -> GLint -> GLint -> IO ())
API_ENTRY(dyn_glutWireCube,ptr_glutWireCube,"glutWireCube",glutWireCube,GLdouble -> IO ())
API_ENTRY(dyn_glutWireCylinder,ptr_glutWireCylinder,"glutWireCylinder",glutWireCylinder,GLdouble -> GLdouble -> GLint -> GLint -> IO ())
API_ENTRY(dyn_glutWireDodecahedron,ptr_glutWireDodecahedron,"glutWireDodecahedron",glutWireDodecahedron,IO ())
API_ENTRY(dyn_glutWireIcosahedron,ptr_glutWireIcosahedron,"glutWireIcosahedron",glutWireIcosahedron,IO ())
API_ENTRY(dyn_glutWireOctahedron,ptr_glutWireOctahedron,"glutWireOctahedron",glutWireOctahedron,IO ())
API_ENTRY(dyn_glutWireRhombicDodecahedron,ptr_glutWireRhombicDodecahedron,"glutWireRhombicDodecahedron",glutWireRhombicDodecahedron,IO ())
API_ENTRY(dyn_glutWireSierpinskiSponge,ptr_glutWireSierpinskiSponge,"glutWireSierpinskiSponge",glutWireSierpinskiSponge,CInt -> Ptr GLdouble -> GLdouble -> IO ())
API_ENTRY(dyn_glutWireSphere,ptr_glutWireSphere,"glutWireSphere",glutWireSphere,GLdouble -> GLint -> GLint -> IO ())
API_ENTRY(dyn_glutWireTeacup,ptr_glutWireTeacup,"glutWireTeacup",glutWireTeacup,GLdouble -> IO ())
API_ENTRY(dyn_glutWireTeapot,ptr_glutWireTeapot,"glutWireTeapot",glutWireTeapot,GLdouble -> IO ())
API_ENTRY(dyn_glutWireTeaspoon,ptr_glutWireTeaspoon,"glutWireTeaspoon",glutWireTeaspoon,GLdouble -> IO ())
API_ENTRY(dyn_glutWireTetrahedron,ptr_glutWireTetrahedron,"glutWireTetrahedron",glutWireTetrahedron,IO ())
API_ENTRY(dyn_glutWireTorus,ptr_glutWireTorus,"glutWireTorus",glutWireTorus,GLdouble -> GLdouble -> GLint -> GLint -> IO ())
