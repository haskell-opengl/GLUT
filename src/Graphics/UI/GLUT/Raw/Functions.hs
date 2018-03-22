{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Raw.Functions
-- Copyright   :  (c) Sven Panne 2018
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- All raw functions from GLUT and freeglut.
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.Raw.Functions (
   isKnown,

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

-- Make the foreign imports happy.
import Foreign.C.Types

import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.C.String ( withCString, CString )
import Foreign.Marshal.Error ( throwIf )
import Foreign.Ptr ( Ptr, FunPtr, nullFunPtr )
import Graphics.Rendering.OpenGL ( GLdouble, GLenum, GLfloat, GLint )
import System.IO.Unsafe ( unsafePerformIO )

import Graphics.UI.GLUT.Raw.Callbacks

--------------------------------------------------------------------------------

-- | Retrieve a GLUT API entry by name. Throws a userError when no entry with
-- the given name was found.
getAPIEntry :: String -> IO (FunPtr a)
getAPIEntry extensionEntry =
  throwIfNullFunPtr ("unknown GLUT entry " ++ extensionEntry) $
    getAPIEntryInternal extensionEntry

throwIfNullFunPtr :: String -> IO (FunPtr a) -> IO (FunPtr a)
throwIfNullFunPtr = throwIf (== nullFunPtr) . const

getAPIEntryInternal :: String -> IO (FunPtr a)
getAPIEntryInternal extensionEntry =
   withCString extensionEntry hs_GLUT_getProcAddress

isKnown :: MonadIO m => String -> m Bool
isKnown = liftIO . fmap (/= nullFunPtr) . getAPIEntryInternal

foreign import ccall unsafe "hs_GLUT_getProcAddress"
  hs_GLUT_getProcAddress :: CString -> IO (FunPtr a)

-- glutAddMenuEntry ------------------------------------------------------------

glutAddMenuEntry :: MonadIO m => Ptr CChar -> CInt -> m ()
glutAddMenuEntry v1 v2 = liftIO $ dyn_glutAddMenuEntry ptr_glutAddMenuEntry v1 v2

foreign import CALLCONV "dynamic" dyn_glutAddMenuEntry
  :: FunPtr (Ptr CChar -> CInt -> IO ())
  ->         Ptr CChar -> CInt -> IO ()

{-# NOINLINE ptr_glutAddMenuEntry #-}
ptr_glutAddMenuEntry :: FunPtr a
ptr_glutAddMenuEntry = unsafePerformIO $ getAPIEntry "glutAddMenuEntry"

-- glutAddSubMenu --------------------------------------------------------------

glutAddSubMenu :: MonadIO m => Ptr CChar -> CInt -> m ()
glutAddSubMenu v1 v2 = liftIO $ dyn_glutAddSubMenu ptr_glutAddSubMenu v1 v2

foreign import CALLCONV "dynamic" dyn_glutAddSubMenu
  :: FunPtr (Ptr CChar -> CInt -> IO ())
  ->         Ptr CChar -> CInt -> IO ()

{-# NOINLINE ptr_glutAddSubMenu #-}
ptr_glutAddSubMenu :: FunPtr a
ptr_glutAddSubMenu = unsafePerformIO $ getAPIEntry "glutAddSubMenu"

-- glutAppStatusFunc -----------------------------------------------------------

glutAppStatusFunc :: MonadIO m => FunPtr AppStatusFunc -> m ()
glutAppStatusFunc v1 = liftIO $ dyn_glutAppStatusFunc ptr_glutAppStatusFunc v1

foreign import CALLCONV "dynamic" dyn_glutAppStatusFunc
  :: FunPtr (FunPtr AppStatusFunc -> IO ())
  ->         FunPtr AppStatusFunc -> IO ()

{-# NOINLINE ptr_glutAppStatusFunc #-}
ptr_glutAppStatusFunc :: FunPtr a
ptr_glutAppStatusFunc = unsafePerformIO $ getAPIEntry "glutAppStatusFunc"

-- glutAttachMenu --------------------------------------------------------------

glutAttachMenu :: MonadIO m => CInt -> m ()
glutAttachMenu v1 = liftIO $ dyn_glutAttachMenu ptr_glutAttachMenu v1

foreign import CALLCONV "dynamic" dyn_glutAttachMenu
  :: FunPtr (CInt -> IO ())
  ->         CInt -> IO ()

{-# NOINLINE ptr_glutAttachMenu #-}
ptr_glutAttachMenu :: FunPtr a
ptr_glutAttachMenu = unsafePerformIO $ getAPIEntry "glutAttachMenu"

-- glutBitmapCharacter ---------------------------------------------------------

glutBitmapCharacter :: MonadIO m => Ptr a -> CInt -> m ()
glutBitmapCharacter v1 v2 = liftIO $ dyn_glutBitmapCharacter ptr_glutBitmapCharacter v1 v2

foreign import CALLCONV "dynamic" dyn_glutBitmapCharacter
  :: FunPtr (Ptr a -> CInt -> IO ())
  ->         Ptr a -> CInt -> IO ()

{-# NOINLINE ptr_glutBitmapCharacter #-}
ptr_glutBitmapCharacter :: FunPtr a
ptr_glutBitmapCharacter = unsafePerformIO $ getAPIEntry "glutBitmapCharacter"

-- glutBitmapHeight ------------------------------------------------------------

glutBitmapHeight :: MonadIO m => Ptr a -> m CInt
glutBitmapHeight v1 = liftIO $ dyn_glutBitmapHeight ptr_glutBitmapHeight v1

foreign import CALLCONV "dynamic" dyn_glutBitmapHeight
  :: FunPtr (Ptr a -> IO CInt)
  ->         Ptr a -> IO CInt

{-# NOINLINE ptr_glutBitmapHeight #-}
ptr_glutBitmapHeight :: FunPtr a
ptr_glutBitmapHeight = unsafePerformIO $ getAPIEntry "glutBitmapHeight"

-- glutBitmapLength ------------------------------------------------------------

glutBitmapLength :: MonadIO m => Ptr a -> Ptr CUChar -> m CInt
glutBitmapLength v1 v2 = liftIO $ dyn_glutBitmapLength ptr_glutBitmapLength v1 v2

foreign import CALLCONV "dynamic" dyn_glutBitmapLength
  :: FunPtr (Ptr a -> Ptr CUChar -> IO CInt)
  ->         Ptr a -> Ptr CUChar -> IO CInt

{-# NOINLINE ptr_glutBitmapLength #-}
ptr_glutBitmapLength :: FunPtr a
ptr_glutBitmapLength = unsafePerformIO $ getAPIEntry "glutBitmapLength"

-- glutBitmapString ------------------------------------------------------------

glutBitmapString :: MonadIO m => Ptr a -> Ptr CUChar -> m ()
glutBitmapString v1 v2 = liftIO $ dyn_glutBitmapString ptr_glutBitmapString v1 v2

foreign import CALLCONV "dynamic" dyn_glutBitmapString
  :: FunPtr (Ptr a -> Ptr CUChar -> IO ())
  ->         Ptr a -> Ptr CUChar -> IO ()

{-# NOINLINE ptr_glutBitmapString #-}
ptr_glutBitmapString :: FunPtr a
ptr_glutBitmapString = unsafePerformIO $ getAPIEntry "glutBitmapString"

-- glutBitmapWidth -------------------------------------------------------------

glutBitmapWidth :: MonadIO m => Ptr a -> CInt -> m CInt
glutBitmapWidth v1 v2 = liftIO $ dyn_glutBitmapWidth ptr_glutBitmapWidth v1 v2

foreign import CALLCONV "dynamic" dyn_glutBitmapWidth
  :: FunPtr (Ptr a -> CInt -> IO CInt)
  ->         Ptr a -> CInt -> IO CInt

{-# NOINLINE ptr_glutBitmapWidth #-}
ptr_glutBitmapWidth :: FunPtr a
ptr_glutBitmapWidth = unsafePerformIO $ getAPIEntry "glutBitmapWidth"

-- glutButtonBoxFunc -----------------------------------------------------------

glutButtonBoxFunc :: MonadIO m => FunPtr ButtonBoxFunc -> m ()
glutButtonBoxFunc v1 = liftIO $ dyn_glutButtonBoxFunc ptr_glutButtonBoxFunc v1

foreign import CALLCONV "dynamic" dyn_glutButtonBoxFunc
  :: FunPtr (FunPtr ButtonBoxFunc -> IO ())
  ->         FunPtr ButtonBoxFunc -> IO ()

{-# NOINLINE ptr_glutButtonBoxFunc #-}
ptr_glutButtonBoxFunc :: FunPtr a
ptr_glutButtonBoxFunc = unsafePerformIO $ getAPIEntry "glutButtonBoxFunc"

-- glutChangeToMenuEntry -------------------------------------------------------

glutChangeToMenuEntry :: MonadIO m => CInt -> Ptr CChar -> CInt -> m ()
glutChangeToMenuEntry v1 v2 v3 = liftIO $ dyn_glutChangeToMenuEntry ptr_glutChangeToMenuEntry v1 v2 v3

foreign import CALLCONV "dynamic" dyn_glutChangeToMenuEntry
  :: FunPtr (CInt -> Ptr CChar -> CInt -> IO ())
  ->         CInt -> Ptr CChar -> CInt -> IO ()

{-# NOINLINE ptr_glutChangeToMenuEntry #-}
ptr_glutChangeToMenuEntry :: FunPtr a
ptr_glutChangeToMenuEntry = unsafePerformIO $ getAPIEntry "glutChangeToMenuEntry"

-- glutChangeToSubMenu ---------------------------------------------------------

glutChangeToSubMenu :: MonadIO m => CInt -> Ptr CChar -> CInt -> m ()
glutChangeToSubMenu v1 v2 v3 = liftIO $ dyn_glutChangeToSubMenu ptr_glutChangeToSubMenu v1 v2 v3

foreign import CALLCONV "dynamic" dyn_glutChangeToSubMenu
  :: FunPtr (CInt -> Ptr CChar -> CInt -> IO ())
  ->         CInt -> Ptr CChar -> CInt -> IO ()

{-# NOINLINE ptr_glutChangeToSubMenu #-}
ptr_glutChangeToSubMenu :: FunPtr a
ptr_glutChangeToSubMenu = unsafePerformIO $ getAPIEntry "glutChangeToSubMenu"

-- glutCloseFunc ---------------------------------------------------------------

glutCloseFunc :: MonadIO m => FunPtr CloseFunc -> m ()
glutCloseFunc v1 = liftIO $ dyn_glutCloseFunc ptr_glutCloseFunc v1

foreign import CALLCONV "dynamic" dyn_glutCloseFunc
  :: FunPtr (FunPtr CloseFunc -> IO ())
  ->         FunPtr CloseFunc -> IO ()

{-# NOINLINE ptr_glutCloseFunc #-}
ptr_glutCloseFunc :: FunPtr a
ptr_glutCloseFunc = unsafePerformIO $ getAPIEntry "glutCloseFunc"

-- glutCopyColormap ------------------------------------------------------------

glutCopyColormap :: MonadIO m => CInt -> m ()
glutCopyColormap v1 = liftIO $ dyn_glutCopyColormap ptr_glutCopyColormap v1

foreign import CALLCONV "dynamic" dyn_glutCopyColormap
  :: FunPtr (CInt -> IO ())
  ->         CInt -> IO ()

{-# NOINLINE ptr_glutCopyColormap #-}
ptr_glutCopyColormap :: FunPtr a
ptr_glutCopyColormap = unsafePerformIO $ getAPIEntry "glutCopyColormap"

-- glutCreateMenu --------------------------------------------------------------

glutCreateMenu :: MonadIO m => FunPtr MenuFunc -> m CInt
glutCreateMenu v1 = liftIO $ dyn_glutCreateMenu ptr_glutCreateMenu v1

foreign import CALLCONV "dynamic" dyn_glutCreateMenu
  :: FunPtr (FunPtr MenuFunc -> IO CInt)
  ->         FunPtr MenuFunc -> IO CInt

{-# NOINLINE ptr_glutCreateMenu #-}
ptr_glutCreateMenu :: FunPtr a
ptr_glutCreateMenu = unsafePerformIO $ getAPIEntry "glutCreateMenu"

-- glutCreateSubWindow ---------------------------------------------------------

glutCreateSubWindow :: MonadIO m => CInt -> CInt -> CInt -> CInt -> CInt -> m CInt
glutCreateSubWindow v1 v2 v3 v4 v5 = liftIO $ dyn_glutCreateSubWindow ptr_glutCreateSubWindow v1 v2 v3 v4 v5

foreign import CALLCONV "dynamic" dyn_glutCreateSubWindow
  :: FunPtr (CInt -> CInt -> CInt -> CInt -> CInt -> IO CInt)
  ->         CInt -> CInt -> CInt -> CInt -> CInt -> IO CInt

{-# NOINLINE ptr_glutCreateSubWindow #-}
ptr_glutCreateSubWindow :: FunPtr a
ptr_glutCreateSubWindow = unsafePerformIO $ getAPIEntry "glutCreateSubWindow"

-- glutCreateWindow ------------------------------------------------------------

glutCreateWindow :: MonadIO m => Ptr CChar -> m CInt
glutCreateWindow v1 = liftIO $ dyn_glutCreateWindow ptr_glutCreateWindow v1

foreign import CALLCONV "dynamic" dyn_glutCreateWindow
  :: FunPtr (Ptr CChar -> IO CInt)
  ->         Ptr CChar -> IO CInt

{-# NOINLINE ptr_glutCreateWindow #-}
ptr_glutCreateWindow :: FunPtr a
ptr_glutCreateWindow = unsafePerformIO $ getAPIEntry "glutCreateWindow"

-- glutDestroyMenu -------------------------------------------------------------

glutDestroyMenu :: MonadIO m => CInt -> m ()
glutDestroyMenu v1 = liftIO $ dyn_glutDestroyMenu ptr_glutDestroyMenu v1

foreign import CALLCONV "dynamic" dyn_glutDestroyMenu
  :: FunPtr (CInt -> IO ())
  ->         CInt -> IO ()

{-# NOINLINE ptr_glutDestroyMenu #-}
ptr_glutDestroyMenu :: FunPtr a
ptr_glutDestroyMenu = unsafePerformIO $ getAPIEntry "glutDestroyMenu"

-- glutDestroyWindow -----------------------------------------------------------

glutDestroyWindow :: MonadIO m => CInt -> m ()
glutDestroyWindow v1 = liftIO $ dyn_glutDestroyWindow ptr_glutDestroyWindow v1

foreign import CALLCONV "dynamic" dyn_glutDestroyWindow
  :: FunPtr (CInt -> IO ())
  ->         CInt -> IO ()

{-# NOINLINE ptr_glutDestroyWindow #-}
ptr_glutDestroyWindow :: FunPtr a
ptr_glutDestroyWindow = unsafePerformIO $ getAPIEntry "glutDestroyWindow"

-- glutDetachMenu --------------------------------------------------------------

glutDetachMenu :: MonadIO m => CInt -> m ()
glutDetachMenu v1 = liftIO $ dyn_glutDetachMenu ptr_glutDetachMenu v1

foreign import CALLCONV "dynamic" dyn_glutDetachMenu
  :: FunPtr (CInt -> IO ())
  ->         CInt -> IO ()

{-# NOINLINE ptr_glutDetachMenu #-}
ptr_glutDetachMenu :: FunPtr a
ptr_glutDetachMenu = unsafePerformIO $ getAPIEntry "glutDetachMenu"

-- glutDeviceGet ---------------------------------------------------------------

glutDeviceGet :: MonadIO m => GLenum -> m CInt
glutDeviceGet v1 = liftIO $ dyn_glutDeviceGet ptr_glutDeviceGet v1

foreign import CALLCONV "dynamic" dyn_glutDeviceGet
  :: FunPtr (GLenum -> IO CInt)
  ->         GLenum -> IO CInt

{-# NOINLINE ptr_glutDeviceGet #-}
ptr_glutDeviceGet :: FunPtr a
ptr_glutDeviceGet = unsafePerformIO $ getAPIEntry "glutDeviceGet"

-- glutDialsFunc ---------------------------------------------------------------

glutDialsFunc :: MonadIO m => FunPtr DialsFunc -> m ()
glutDialsFunc v1 = liftIO $ dyn_glutDialsFunc ptr_glutDialsFunc v1

foreign import CALLCONV "dynamic" dyn_glutDialsFunc
  :: FunPtr (FunPtr DialsFunc -> IO ())
  ->         FunPtr DialsFunc -> IO ()

{-# NOINLINE ptr_glutDialsFunc #-}
ptr_glutDialsFunc :: FunPtr a
ptr_glutDialsFunc = unsafePerformIO $ getAPIEntry "glutDialsFunc"

-- glutDisplayFunc -------------------------------------------------------------

glutDisplayFunc :: MonadIO m => FunPtr DisplayFunc -> m ()
glutDisplayFunc v1 = liftIO $ dyn_glutDisplayFunc ptr_glutDisplayFunc v1

foreign import CALLCONV "dynamic" dyn_glutDisplayFunc
  :: FunPtr (FunPtr DisplayFunc -> IO ())
  ->         FunPtr DisplayFunc -> IO ()

{-# NOINLINE ptr_glutDisplayFunc #-}
ptr_glutDisplayFunc :: FunPtr a
ptr_glutDisplayFunc = unsafePerformIO $ getAPIEntry "glutDisplayFunc"

-- glutEnterGameMode -----------------------------------------------------------

glutEnterGameMode :: MonadIO m => m CInt
glutEnterGameMode = liftIO $ dyn_glutEnterGameMode ptr_glutEnterGameMode

foreign import CALLCONV "dynamic" dyn_glutEnterGameMode
  :: FunPtr (IO CInt)
  ->         IO CInt

{-# NOINLINE ptr_glutEnterGameMode #-}
ptr_glutEnterGameMode :: FunPtr a
ptr_glutEnterGameMode = unsafePerformIO $ getAPIEntry "glutEnterGameMode"

-- glutEntryFunc ---------------------------------------------------------------

glutEntryFunc :: MonadIO m => FunPtr EntryFunc -> m ()
glutEntryFunc v1 = liftIO $ dyn_glutEntryFunc ptr_glutEntryFunc v1

foreign import CALLCONV "dynamic" dyn_glutEntryFunc
  :: FunPtr (FunPtr EntryFunc -> IO ())
  ->         FunPtr EntryFunc -> IO ()

{-# NOINLINE ptr_glutEntryFunc #-}
ptr_glutEntryFunc :: FunPtr a
ptr_glutEntryFunc = unsafePerformIO $ getAPIEntry "glutEntryFunc"

-- glutEstablishOverlay --------------------------------------------------------

glutEstablishOverlay :: MonadIO m => m ()
glutEstablishOverlay = liftIO $ dyn_glutEstablishOverlay ptr_glutEstablishOverlay

foreign import CALLCONV "dynamic" dyn_glutEstablishOverlay
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutEstablishOverlay #-}
ptr_glutEstablishOverlay :: FunPtr a
ptr_glutEstablishOverlay = unsafePerformIO $ getAPIEntry "glutEstablishOverlay"

-- glutExit --------------------------------------------------------------------

glutExit :: MonadIO m => m ()
glutExit = liftIO $ dyn_glutExit ptr_glutExit

foreign import CALLCONV "dynamic" dyn_glutExit
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutExit #-}
ptr_glutExit :: FunPtr a
ptr_glutExit = unsafePerformIO $ getAPIEntry "glutExit"

-- glutExtensionSupported ------------------------------------------------------

glutExtensionSupported :: MonadIO m => Ptr CChar -> m CInt
glutExtensionSupported v1 = liftIO $ dyn_glutExtensionSupported ptr_glutExtensionSupported v1

foreign import CALLCONV "dynamic" dyn_glutExtensionSupported
  :: FunPtr (Ptr CChar -> IO CInt)
  ->         Ptr CChar -> IO CInt

{-# NOINLINE ptr_glutExtensionSupported #-}
ptr_glutExtensionSupported :: FunPtr a
ptr_glutExtensionSupported = unsafePerformIO $ getAPIEntry "glutExtensionSupported"

-- glutForceJoystickFunc -------------------------------------------------------

glutForceJoystickFunc :: MonadIO m => m ()
glutForceJoystickFunc = liftIO $ dyn_glutForceJoystickFunc ptr_glutForceJoystickFunc

foreign import CALLCONV "dynamic" dyn_glutForceJoystickFunc
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutForceJoystickFunc #-}
ptr_glutForceJoystickFunc :: FunPtr a
ptr_glutForceJoystickFunc = unsafePerformIO $ getAPIEntry "glutForceJoystickFunc"

-- glutFullScreen --------------------------------------------------------------

glutFullScreen :: MonadIO m => m ()
glutFullScreen = liftIO $ dyn_glutFullScreen ptr_glutFullScreen

foreign import CALLCONV "dynamic" dyn_glutFullScreen
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutFullScreen #-}
ptr_glutFullScreen :: FunPtr a
ptr_glutFullScreen = unsafePerformIO $ getAPIEntry "glutFullScreen"

-- glutFullScreenToggle --------------------------------------------------------

glutFullScreenToggle :: MonadIO m => m ()
glutFullScreenToggle = liftIO $ dyn_glutFullScreenToggle ptr_glutFullScreenToggle

foreign import CALLCONV "dynamic" dyn_glutFullScreenToggle
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutFullScreenToggle #-}
ptr_glutFullScreenToggle :: FunPtr a
ptr_glutFullScreenToggle = unsafePerformIO $ getAPIEntry "glutFullScreenToggle"

-- glutGameModeGet -------------------------------------------------------------

glutGameModeGet :: MonadIO m => GLenum -> m CInt
glutGameModeGet v1 = liftIO $ dyn_glutGameModeGet ptr_glutGameModeGet v1

foreign import CALLCONV "dynamic" dyn_glutGameModeGet
  :: FunPtr (GLenum -> IO CInt)
  ->         GLenum -> IO CInt

{-# NOINLINE ptr_glutGameModeGet #-}
ptr_glutGameModeGet :: FunPtr a
ptr_glutGameModeGet = unsafePerformIO $ getAPIEntry "glutGameModeGet"

-- glutGameModeString ----------------------------------------------------------

glutGameModeString :: MonadIO m => Ptr CChar -> m ()
glutGameModeString v1 = liftIO $ dyn_glutGameModeString ptr_glutGameModeString v1

foreign import CALLCONV "dynamic" dyn_glutGameModeString
  :: FunPtr (Ptr CChar -> IO ())
  ->         Ptr CChar -> IO ()

{-# NOINLINE ptr_glutGameModeString #-}
ptr_glutGameModeString :: FunPtr a
ptr_glutGameModeString = unsafePerformIO $ getAPIEntry "glutGameModeString"

-- glutGet ---------------------------------------------------------------------

glutGet :: MonadIO m => GLenum -> m CInt
glutGet v1 = liftIO $ dyn_glutGet ptr_glutGet v1

foreign import CALLCONV "dynamic" dyn_glutGet
  :: FunPtr (GLenum -> IO CInt)
  ->         GLenum -> IO CInt

{-# NOINLINE ptr_glutGet #-}
ptr_glutGet :: FunPtr a
ptr_glutGet = unsafePerformIO $ getAPIEntry "glutGet"

-- glutGetColor ----------------------------------------------------------------

glutGetColor :: MonadIO m => CInt -> CInt -> m GLfloat
glutGetColor v1 v2 = liftIO $ dyn_glutGetColor ptr_glutGetColor v1 v2

foreign import CALLCONV "dynamic" dyn_glutGetColor
  :: FunPtr (CInt -> CInt -> IO GLfloat)
  ->         CInt -> CInt -> IO GLfloat

{-# NOINLINE ptr_glutGetColor #-}
ptr_glutGetColor :: FunPtr a
ptr_glutGetColor = unsafePerformIO $ getAPIEntry "glutGetColor"

-- glutGetMenu -----------------------------------------------------------------

glutGetMenu :: MonadIO m => m CInt
glutGetMenu = liftIO $ dyn_glutGetMenu ptr_glutGetMenu

foreign import CALLCONV "dynamic" dyn_glutGetMenu
  :: FunPtr (IO CInt)
  ->         IO CInt

{-# NOINLINE ptr_glutGetMenu #-}
ptr_glutGetMenu :: FunPtr a
ptr_glutGetMenu = unsafePerformIO $ getAPIEntry "glutGetMenu"

-- glutGetMenuData -------------------------------------------------------------

glutGetMenuData :: MonadIO m => m (Ptr a)
glutGetMenuData = liftIO $ dyn_glutGetMenuData ptr_glutGetMenuData

foreign import CALLCONV "dynamic" dyn_glutGetMenuData
  :: FunPtr (IO (Ptr a))
  ->         IO (Ptr a)

{-# NOINLINE ptr_glutGetMenuData #-}
ptr_glutGetMenuData :: FunPtr a
ptr_glutGetMenuData = unsafePerformIO $ getAPIEntry "glutGetMenuData"

-- glutGetModeValues -----------------------------------------------------------

glutGetModeValues :: MonadIO m => GLenum -> Ptr CInt -> m (Ptr CInt)
glutGetModeValues v1 v2 = liftIO $ dyn_glutGetModeValues ptr_glutGetModeValues v1 v2

foreign import CALLCONV "dynamic" dyn_glutGetModeValues
  :: FunPtr (GLenum -> Ptr CInt -> IO (Ptr CInt))
  ->         GLenum -> Ptr CInt -> IO (Ptr CInt)

{-# NOINLINE ptr_glutGetModeValues #-}
ptr_glutGetModeValues :: FunPtr a
ptr_glutGetModeValues = unsafePerformIO $ getAPIEntry "glutGetModeValues"

-- glutGetModifiers ------------------------------------------------------------

glutGetModifiers :: MonadIO m => m CInt
glutGetModifiers = liftIO $ dyn_glutGetModifiers ptr_glutGetModifiers

foreign import CALLCONV "dynamic" dyn_glutGetModifiers
  :: FunPtr (IO CInt)
  ->         IO CInt

{-# NOINLINE ptr_glutGetModifiers #-}
ptr_glutGetModifiers :: FunPtr a
ptr_glutGetModifiers = unsafePerformIO $ getAPIEntry "glutGetModifiers"

-- glutGetProcAddress ----------------------------------------------------------

glutGetProcAddress :: MonadIO m => Ptr CChar -> m (FunPtr a)
glutGetProcAddress v1 = liftIO $ dyn_glutGetProcAddress ptr_glutGetProcAddress v1

foreign import CALLCONV "dynamic" dyn_glutGetProcAddress
  :: FunPtr (Ptr CChar -> IO (FunPtr a))
  ->         Ptr CChar -> IO (FunPtr a)

{-# NOINLINE ptr_glutGetProcAddress #-}
ptr_glutGetProcAddress :: FunPtr a
ptr_glutGetProcAddress = unsafePerformIO $ getAPIEntry "glutGetProcAddress"

-- glutGetWindow ---------------------------------------------------------------

glutGetWindow :: MonadIO m => m CInt
glutGetWindow = liftIO $ dyn_glutGetWindow ptr_glutGetWindow

foreign import CALLCONV "dynamic" dyn_glutGetWindow
  :: FunPtr (IO CInt)
  ->         IO CInt

{-# NOINLINE ptr_glutGetWindow #-}
ptr_glutGetWindow :: FunPtr a
ptr_glutGetWindow = unsafePerformIO $ getAPIEntry "glutGetWindow"

-- glutGetWindowData -----------------------------------------------------------

glutGetWindowData :: MonadIO m => m (Ptr a)
glutGetWindowData = liftIO $ dyn_glutGetWindowData ptr_glutGetWindowData

foreign import CALLCONV "dynamic" dyn_glutGetWindowData
  :: FunPtr (IO (Ptr a))
  ->         IO (Ptr a)

{-# NOINLINE ptr_glutGetWindowData #-}
ptr_glutGetWindowData :: FunPtr a
ptr_glutGetWindowData = unsafePerformIO $ getAPIEntry "glutGetWindowData"

-- glutHideOverlay -------------------------------------------------------------

glutHideOverlay :: MonadIO m => m ()
glutHideOverlay = liftIO $ dyn_glutHideOverlay ptr_glutHideOverlay

foreign import CALLCONV "dynamic" dyn_glutHideOverlay
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutHideOverlay #-}
ptr_glutHideOverlay :: FunPtr a
ptr_glutHideOverlay = unsafePerformIO $ getAPIEntry "glutHideOverlay"

-- glutHideWindow --------------------------------------------------------------

glutHideWindow :: MonadIO m => m ()
glutHideWindow = liftIO $ dyn_glutHideWindow ptr_glutHideWindow

foreign import CALLCONV "dynamic" dyn_glutHideWindow
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutHideWindow #-}
ptr_glutHideWindow :: FunPtr a
ptr_glutHideWindow = unsafePerformIO $ getAPIEntry "glutHideWindow"

-- glutIconifyWindow -----------------------------------------------------------

glutIconifyWindow :: MonadIO m => m ()
glutIconifyWindow = liftIO $ dyn_glutIconifyWindow ptr_glutIconifyWindow

foreign import CALLCONV "dynamic" dyn_glutIconifyWindow
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutIconifyWindow #-}
ptr_glutIconifyWindow :: FunPtr a
ptr_glutIconifyWindow = unsafePerformIO $ getAPIEntry "glutIconifyWindow"

-- glutIdleFunc ----------------------------------------------------------------

glutIdleFunc :: MonadIO m => FunPtr IdleFunc -> m ()
glutIdleFunc v1 = liftIO $ dyn_glutIdleFunc ptr_glutIdleFunc v1

foreign import CALLCONV "dynamic" dyn_glutIdleFunc
  :: FunPtr (FunPtr IdleFunc -> IO ())
  ->         FunPtr IdleFunc -> IO ()

{-# NOINLINE ptr_glutIdleFunc #-}
ptr_glutIdleFunc :: FunPtr a
ptr_glutIdleFunc = unsafePerformIO $ getAPIEntry "glutIdleFunc"

-- glutIgnoreKeyRepeat ---------------------------------------------------------

glutIgnoreKeyRepeat :: MonadIO m => CInt -> m ()
glutIgnoreKeyRepeat v1 = liftIO $ dyn_glutIgnoreKeyRepeat ptr_glutIgnoreKeyRepeat v1

foreign import CALLCONV "dynamic" dyn_glutIgnoreKeyRepeat
  :: FunPtr (CInt -> IO ())
  ->         CInt -> IO ()

{-# NOINLINE ptr_glutIgnoreKeyRepeat #-}
ptr_glutIgnoreKeyRepeat :: FunPtr a
ptr_glutIgnoreKeyRepeat = unsafePerformIO $ getAPIEntry "glutIgnoreKeyRepeat"

-- glutInit --------------------------------------------------------------------

glutInit :: MonadIO m => Ptr CInt -> Ptr (Ptr CChar) -> m ()
glutInit v1 v2 = liftIO $ dyn_glutInit ptr_glutInit v1 v2

foreign import CALLCONV "dynamic" dyn_glutInit
  :: FunPtr (Ptr CInt -> Ptr (Ptr CChar) -> IO ())
  ->         Ptr CInt -> Ptr (Ptr CChar) -> IO ()

{-# NOINLINE ptr_glutInit #-}
ptr_glutInit :: FunPtr a
ptr_glutInit = unsafePerformIO $ getAPIEntry "glutInit"

-- glutInitContextFlags --------------------------------------------------------

glutInitContextFlags :: MonadIO m => CInt -> m ()
glutInitContextFlags v1 = liftIO $ dyn_glutInitContextFlags ptr_glutInitContextFlags v1

foreign import CALLCONV "dynamic" dyn_glutInitContextFlags
  :: FunPtr (CInt -> IO ())
  ->         CInt -> IO ()

{-# NOINLINE ptr_glutInitContextFlags #-}
ptr_glutInitContextFlags :: FunPtr a
ptr_glutInitContextFlags = unsafePerformIO $ getAPIEntry "glutInitContextFlags"

-- glutInitContextFunc ---------------------------------------------------------

glutInitContextFunc :: MonadIO m => FunPtr InitContextFunc -> m ()
glutInitContextFunc v1 = liftIO $ dyn_glutInitContextFunc ptr_glutInitContextFunc v1

foreign import CALLCONV "dynamic" dyn_glutInitContextFunc
  :: FunPtr (FunPtr InitContextFunc -> IO ())
  ->         FunPtr InitContextFunc -> IO ()

{-# NOINLINE ptr_glutInitContextFunc #-}
ptr_glutInitContextFunc :: FunPtr a
ptr_glutInitContextFunc = unsafePerformIO $ getAPIEntry "glutInitContextFunc"

-- glutInitContextProfile ------------------------------------------------------

glutInitContextProfile :: MonadIO m => CInt -> m ()
glutInitContextProfile v1 = liftIO $ dyn_glutInitContextProfile ptr_glutInitContextProfile v1

foreign import CALLCONV "dynamic" dyn_glutInitContextProfile
  :: FunPtr (CInt -> IO ())
  ->         CInt -> IO ()

{-# NOINLINE ptr_glutInitContextProfile #-}
ptr_glutInitContextProfile :: FunPtr a
ptr_glutInitContextProfile = unsafePerformIO $ getAPIEntry "glutInitContextProfile"

-- glutInitContextVersion ------------------------------------------------------

glutInitContextVersion :: MonadIO m => CInt -> CInt -> m ()
glutInitContextVersion v1 v2 = liftIO $ dyn_glutInitContextVersion ptr_glutInitContextVersion v1 v2

foreign import CALLCONV "dynamic" dyn_glutInitContextVersion
  :: FunPtr (CInt -> CInt -> IO ())
  ->         CInt -> CInt -> IO ()

{-# NOINLINE ptr_glutInitContextVersion #-}
ptr_glutInitContextVersion :: FunPtr a
ptr_glutInitContextVersion = unsafePerformIO $ getAPIEntry "glutInitContextVersion"

-- glutInitDisplayMode ---------------------------------------------------------

glutInitDisplayMode :: MonadIO m => CUInt -> m ()
glutInitDisplayMode v1 = liftIO $ dyn_glutInitDisplayMode ptr_glutInitDisplayMode v1

foreign import CALLCONV "dynamic" dyn_glutInitDisplayMode
  :: FunPtr (CUInt -> IO ())
  ->         CUInt -> IO ()

{-# NOINLINE ptr_glutInitDisplayMode #-}
ptr_glutInitDisplayMode :: FunPtr a
ptr_glutInitDisplayMode = unsafePerformIO $ getAPIEntry "glutInitDisplayMode"

-- glutInitDisplayString -------------------------------------------------------

glutInitDisplayString :: MonadIO m => Ptr CChar -> m ()
glutInitDisplayString v1 = liftIO $ dyn_glutInitDisplayString ptr_glutInitDisplayString v1

foreign import CALLCONV "dynamic" dyn_glutInitDisplayString
  :: FunPtr (Ptr CChar -> IO ())
  ->         Ptr CChar -> IO ()

{-# NOINLINE ptr_glutInitDisplayString #-}
ptr_glutInitDisplayString :: FunPtr a
ptr_glutInitDisplayString = unsafePerformIO $ getAPIEntry "glutInitDisplayString"

-- glutInitWindowPosition ------------------------------------------------------

glutInitWindowPosition :: MonadIO m => CInt -> CInt -> m ()
glutInitWindowPosition v1 v2 = liftIO $ dyn_glutInitWindowPosition ptr_glutInitWindowPosition v1 v2

foreign import CALLCONV "dynamic" dyn_glutInitWindowPosition
  :: FunPtr (CInt -> CInt -> IO ())
  ->         CInt -> CInt -> IO ()

{-# NOINLINE ptr_glutInitWindowPosition #-}
ptr_glutInitWindowPosition :: FunPtr a
ptr_glutInitWindowPosition = unsafePerformIO $ getAPIEntry "glutInitWindowPosition"

-- glutInitWindowSize ----------------------------------------------------------

glutInitWindowSize :: MonadIO m => CInt -> CInt -> m ()
glutInitWindowSize v1 v2 = liftIO $ dyn_glutInitWindowSize ptr_glutInitWindowSize v1 v2

foreign import CALLCONV "dynamic" dyn_glutInitWindowSize
  :: FunPtr (CInt -> CInt -> IO ())
  ->         CInt -> CInt -> IO ()

{-# NOINLINE ptr_glutInitWindowSize #-}
ptr_glutInitWindowSize :: FunPtr a
ptr_glutInitWindowSize = unsafePerformIO $ getAPIEntry "glutInitWindowSize"

-- glutJoystickFunc ------------------------------------------------------------

glutJoystickFunc :: MonadIO m => FunPtr JoystickFunc -> CInt -> m ()
glutJoystickFunc v1 v2 = liftIO $ dyn_glutJoystickFunc ptr_glutJoystickFunc v1 v2

foreign import CALLCONV "dynamic" dyn_glutJoystickFunc
  :: FunPtr (FunPtr JoystickFunc -> CInt -> IO ())
  ->         FunPtr JoystickFunc -> CInt -> IO ()

{-# NOINLINE ptr_glutJoystickFunc #-}
ptr_glutJoystickFunc :: FunPtr a
ptr_glutJoystickFunc = unsafePerformIO $ getAPIEntry "glutJoystickFunc"

-- glutKeyboardFunc ------------------------------------------------------------

glutKeyboardFunc :: MonadIO m => FunPtr KeyboardFunc -> m ()
glutKeyboardFunc v1 = liftIO $ dyn_glutKeyboardFunc ptr_glutKeyboardFunc v1

foreign import CALLCONV "dynamic" dyn_glutKeyboardFunc
  :: FunPtr (FunPtr KeyboardFunc -> IO ())
  ->         FunPtr KeyboardFunc -> IO ()

{-# NOINLINE ptr_glutKeyboardFunc #-}
ptr_glutKeyboardFunc :: FunPtr a
ptr_glutKeyboardFunc = unsafePerformIO $ getAPIEntry "glutKeyboardFunc"

-- glutKeyboardUpFunc ----------------------------------------------------------

glutKeyboardUpFunc :: MonadIO m => FunPtr KeyboardUpFunc -> m ()
glutKeyboardUpFunc v1 = liftIO $ dyn_glutKeyboardUpFunc ptr_glutKeyboardUpFunc v1

foreign import CALLCONV "dynamic" dyn_glutKeyboardUpFunc
  :: FunPtr (FunPtr KeyboardUpFunc -> IO ())
  ->         FunPtr KeyboardUpFunc -> IO ()

{-# NOINLINE ptr_glutKeyboardUpFunc #-}
ptr_glutKeyboardUpFunc :: FunPtr a
ptr_glutKeyboardUpFunc = unsafePerformIO $ getAPIEntry "glutKeyboardUpFunc"

-- glutLayerGet ----------------------------------------------------------------

glutLayerGet :: MonadIO m => GLenum -> m CInt
glutLayerGet v1 = liftIO $ dyn_glutLayerGet ptr_glutLayerGet v1

foreign import CALLCONV "dynamic" dyn_glutLayerGet
  :: FunPtr (GLenum -> IO CInt)
  ->         GLenum -> IO CInt

{-# NOINLINE ptr_glutLayerGet #-}
ptr_glutLayerGet :: FunPtr a
ptr_glutLayerGet = unsafePerformIO $ getAPIEntry "glutLayerGet"

-- glutLeaveFullScreen ---------------------------------------------------------

glutLeaveFullScreen :: MonadIO m => m ()
glutLeaveFullScreen = liftIO $ dyn_glutLeaveFullScreen ptr_glutLeaveFullScreen

foreign import CALLCONV "dynamic" dyn_glutLeaveFullScreen
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutLeaveFullScreen #-}
ptr_glutLeaveFullScreen :: FunPtr a
ptr_glutLeaveFullScreen = unsafePerformIO $ getAPIEntry "glutLeaveFullScreen"

-- glutLeaveGameMode -----------------------------------------------------------

glutLeaveGameMode :: MonadIO m => m ()
glutLeaveGameMode = liftIO $ dyn_glutLeaveGameMode ptr_glutLeaveGameMode

foreign import CALLCONV "dynamic" dyn_glutLeaveGameMode
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutLeaveGameMode #-}
ptr_glutLeaveGameMode :: FunPtr a
ptr_glutLeaveGameMode = unsafePerformIO $ getAPIEntry "glutLeaveGameMode"

-- glutLeaveMainLoop -----------------------------------------------------------

glutLeaveMainLoop :: MonadIO m => m ()
glutLeaveMainLoop = liftIO $ dyn_glutLeaveMainLoop ptr_glutLeaveMainLoop

foreign import CALLCONV "dynamic" dyn_glutLeaveMainLoop
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutLeaveMainLoop #-}
ptr_glutLeaveMainLoop :: FunPtr a
ptr_glutLeaveMainLoop = unsafePerformIO $ getAPIEntry "glutLeaveMainLoop"

-- glutMainLoop ----------------------------------------------------------------

glutMainLoop :: MonadIO m => m ()
glutMainLoop = liftIO $ dyn_glutMainLoop ptr_glutMainLoop

foreign import CALLCONV "dynamic" dyn_glutMainLoop
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutMainLoop #-}
ptr_glutMainLoop :: FunPtr a
ptr_glutMainLoop = unsafePerformIO $ getAPIEntry "glutMainLoop"

-- glutMainLoopEvent -----------------------------------------------------------

glutMainLoopEvent :: MonadIO m => m ()
glutMainLoopEvent = liftIO $ dyn_glutMainLoopEvent ptr_glutMainLoopEvent

foreign import CALLCONV "dynamic" dyn_glutMainLoopEvent
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutMainLoopEvent #-}
ptr_glutMainLoopEvent :: FunPtr a
ptr_glutMainLoopEvent = unsafePerformIO $ getAPIEntry "glutMainLoopEvent"

-- glutMenuDestroyFunc ---------------------------------------------------------

glutMenuDestroyFunc :: MonadIO m => FunPtr MenuDestroyFunc -> m ()
glutMenuDestroyFunc v1 = liftIO $ dyn_glutMenuDestroyFunc ptr_glutMenuDestroyFunc v1

foreign import CALLCONV "dynamic" dyn_glutMenuDestroyFunc
  :: FunPtr (FunPtr MenuDestroyFunc -> IO ())
  ->         FunPtr MenuDestroyFunc -> IO ()

{-# NOINLINE ptr_glutMenuDestroyFunc #-}
ptr_glutMenuDestroyFunc :: FunPtr a
ptr_glutMenuDestroyFunc = unsafePerformIO $ getAPIEntry "glutMenuDestroyFunc"

-- glutMenuStateFunc -----------------------------------------------------------

glutMenuStateFunc :: MonadIO m => FunPtr MenuStateFunc -> m ()
glutMenuStateFunc v1 = liftIO $ dyn_glutMenuStateFunc ptr_glutMenuStateFunc v1

foreign import CALLCONV "dynamic" dyn_glutMenuStateFunc
  :: FunPtr (FunPtr MenuStateFunc -> IO ())
  ->         FunPtr MenuStateFunc -> IO ()

{-# NOINLINE ptr_glutMenuStateFunc #-}
ptr_glutMenuStateFunc :: FunPtr a
ptr_glutMenuStateFunc = unsafePerformIO $ getAPIEntry "glutMenuStateFunc"

-- glutMenuStatusFunc ----------------------------------------------------------

glutMenuStatusFunc :: MonadIO m => FunPtr MenuStatusFunc -> m ()
glutMenuStatusFunc v1 = liftIO $ dyn_glutMenuStatusFunc ptr_glutMenuStatusFunc v1

foreign import CALLCONV "dynamic" dyn_glutMenuStatusFunc
  :: FunPtr (FunPtr MenuStatusFunc -> IO ())
  ->         FunPtr MenuStatusFunc -> IO ()

{-# NOINLINE ptr_glutMenuStatusFunc #-}
ptr_glutMenuStatusFunc :: FunPtr a
ptr_glutMenuStatusFunc = unsafePerformIO $ getAPIEntry "glutMenuStatusFunc"

-- glutMotionFunc --------------------------------------------------------------

glutMotionFunc :: MonadIO m => FunPtr MotionFunc -> m ()
glutMotionFunc v1 = liftIO $ dyn_glutMotionFunc ptr_glutMotionFunc v1

foreign import CALLCONV "dynamic" dyn_glutMotionFunc
  :: FunPtr (FunPtr MotionFunc -> IO ())
  ->         FunPtr MotionFunc -> IO ()

{-# NOINLINE ptr_glutMotionFunc #-}
ptr_glutMotionFunc :: FunPtr a
ptr_glutMotionFunc = unsafePerformIO $ getAPIEntry "glutMotionFunc"

-- glutMouseFunc ---------------------------------------------------------------

glutMouseFunc :: MonadIO m => FunPtr MouseFunc -> m ()
glutMouseFunc v1 = liftIO $ dyn_glutMouseFunc ptr_glutMouseFunc v1

foreign import CALLCONV "dynamic" dyn_glutMouseFunc
  :: FunPtr (FunPtr MouseFunc -> IO ())
  ->         FunPtr MouseFunc -> IO ()

{-# NOINLINE ptr_glutMouseFunc #-}
ptr_glutMouseFunc :: FunPtr a
ptr_glutMouseFunc = unsafePerformIO $ getAPIEntry "glutMouseFunc"

-- glutMouseWheelFunc ----------------------------------------------------------

glutMouseWheelFunc :: MonadIO m => FunPtr MouseWheelFunc -> m ()
glutMouseWheelFunc v1 = liftIO $ dyn_glutMouseWheelFunc ptr_glutMouseWheelFunc v1

foreign import CALLCONV "dynamic" dyn_glutMouseWheelFunc
  :: FunPtr (FunPtr MouseWheelFunc -> IO ())
  ->         FunPtr MouseWheelFunc -> IO ()

{-# NOINLINE ptr_glutMouseWheelFunc #-}
ptr_glutMouseWheelFunc :: FunPtr a
ptr_glutMouseWheelFunc = unsafePerformIO $ getAPIEntry "glutMouseWheelFunc"

-- glutMultiButtonFunc ---------------------------------------------------------

glutMultiButtonFunc :: MonadIO m => FunPtr MultiButtonFunc -> m ()
glutMultiButtonFunc v1 = liftIO $ dyn_glutMultiButtonFunc ptr_glutMultiButtonFunc v1

foreign import CALLCONV "dynamic" dyn_glutMultiButtonFunc
  :: FunPtr (FunPtr MultiButtonFunc -> IO ())
  ->         FunPtr MultiButtonFunc -> IO ()

{-# NOINLINE ptr_glutMultiButtonFunc #-}
ptr_glutMultiButtonFunc :: FunPtr a
ptr_glutMultiButtonFunc = unsafePerformIO $ getAPIEntry "glutMultiButtonFunc"

-- glutMultiEntryFunc ----------------------------------------------------------

glutMultiEntryFunc :: MonadIO m => FunPtr MultiEntryFunc -> m ()
glutMultiEntryFunc v1 = liftIO $ dyn_glutMultiEntryFunc ptr_glutMultiEntryFunc v1

foreign import CALLCONV "dynamic" dyn_glutMultiEntryFunc
  :: FunPtr (FunPtr MultiEntryFunc -> IO ())
  ->         FunPtr MultiEntryFunc -> IO ()

{-# NOINLINE ptr_glutMultiEntryFunc #-}
ptr_glutMultiEntryFunc :: FunPtr a
ptr_glutMultiEntryFunc = unsafePerformIO $ getAPIEntry "glutMultiEntryFunc"

-- glutMultiMotionFunc ---------------------------------------------------------

glutMultiMotionFunc :: MonadIO m => FunPtr MultiMotionFunc -> m ()
glutMultiMotionFunc v1 = liftIO $ dyn_glutMultiMotionFunc ptr_glutMultiMotionFunc v1

foreign import CALLCONV "dynamic" dyn_glutMultiMotionFunc
  :: FunPtr (FunPtr MultiMotionFunc -> IO ())
  ->         FunPtr MultiMotionFunc -> IO ()

{-# NOINLINE ptr_glutMultiMotionFunc #-}
ptr_glutMultiMotionFunc :: FunPtr a
ptr_glutMultiMotionFunc = unsafePerformIO $ getAPIEntry "glutMultiMotionFunc"

-- glutMultiPassiveFunc --------------------------------------------------------

glutMultiPassiveFunc :: MonadIO m => FunPtr MultiPassiveFunc -> m ()
glutMultiPassiveFunc v1 = liftIO $ dyn_glutMultiPassiveFunc ptr_glutMultiPassiveFunc v1

foreign import CALLCONV "dynamic" dyn_glutMultiPassiveFunc
  :: FunPtr (FunPtr MultiPassiveFunc -> IO ())
  ->         FunPtr MultiPassiveFunc -> IO ()

{-# NOINLINE ptr_glutMultiPassiveFunc #-}
ptr_glutMultiPassiveFunc :: FunPtr a
ptr_glutMultiPassiveFunc = unsafePerformIO $ getAPIEntry "glutMultiPassiveFunc"

-- glutOverlayDisplayFunc ------------------------------------------------------

glutOverlayDisplayFunc :: MonadIO m => FunPtr OverlayDisplayFunc -> m ()
glutOverlayDisplayFunc v1 = liftIO $ dyn_glutOverlayDisplayFunc ptr_glutOverlayDisplayFunc v1

foreign import CALLCONV "dynamic" dyn_glutOverlayDisplayFunc
  :: FunPtr (FunPtr OverlayDisplayFunc -> IO ())
  ->         FunPtr OverlayDisplayFunc -> IO ()

{-# NOINLINE ptr_glutOverlayDisplayFunc #-}
ptr_glutOverlayDisplayFunc :: FunPtr a
ptr_glutOverlayDisplayFunc = unsafePerformIO $ getAPIEntry "glutOverlayDisplayFunc"

-- glutPassiveMotionFunc -------------------------------------------------------

glutPassiveMotionFunc :: MonadIO m => FunPtr PassiveMotionFunc -> m ()
glutPassiveMotionFunc v1 = liftIO $ dyn_glutPassiveMotionFunc ptr_glutPassiveMotionFunc v1

foreign import CALLCONV "dynamic" dyn_glutPassiveMotionFunc
  :: FunPtr (FunPtr PassiveMotionFunc -> IO ())
  ->         FunPtr PassiveMotionFunc -> IO ()

{-# NOINLINE ptr_glutPassiveMotionFunc #-}
ptr_glutPassiveMotionFunc :: FunPtr a
ptr_glutPassiveMotionFunc = unsafePerformIO $ getAPIEntry "glutPassiveMotionFunc"

-- glutPopWindow ---------------------------------------------------------------

glutPopWindow :: MonadIO m => m ()
glutPopWindow = liftIO $ dyn_glutPopWindow ptr_glutPopWindow

foreign import CALLCONV "dynamic" dyn_glutPopWindow
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutPopWindow #-}
ptr_glutPopWindow :: FunPtr a
ptr_glutPopWindow = unsafePerformIO $ getAPIEntry "glutPopWindow"

-- glutPositionFunc ------------------------------------------------------------

glutPositionFunc :: MonadIO m => FunPtr PositionFunc -> m ()
glutPositionFunc v1 = liftIO $ dyn_glutPositionFunc ptr_glutPositionFunc v1

foreign import CALLCONV "dynamic" dyn_glutPositionFunc
  :: FunPtr (FunPtr PositionFunc -> IO ())
  ->         FunPtr PositionFunc -> IO ()

{-# NOINLINE ptr_glutPositionFunc #-}
ptr_glutPositionFunc :: FunPtr a
ptr_glutPositionFunc = unsafePerformIO $ getAPIEntry "glutPositionFunc"

-- glutPositionWindow ----------------------------------------------------------

glutPositionWindow :: MonadIO m => CInt -> CInt -> m ()
glutPositionWindow v1 v2 = liftIO $ dyn_glutPositionWindow ptr_glutPositionWindow v1 v2

foreign import CALLCONV "dynamic" dyn_glutPositionWindow
  :: FunPtr (CInt -> CInt -> IO ())
  ->         CInt -> CInt -> IO ()

{-# NOINLINE ptr_glutPositionWindow #-}
ptr_glutPositionWindow :: FunPtr a
ptr_glutPositionWindow = unsafePerformIO $ getAPIEntry "glutPositionWindow"

-- glutPostOverlayRedisplay ----------------------------------------------------

glutPostOverlayRedisplay :: MonadIO m => m ()
glutPostOverlayRedisplay = liftIO $ dyn_glutPostOverlayRedisplay ptr_glutPostOverlayRedisplay

foreign import CALLCONV "dynamic" dyn_glutPostOverlayRedisplay
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutPostOverlayRedisplay #-}
ptr_glutPostOverlayRedisplay :: FunPtr a
ptr_glutPostOverlayRedisplay = unsafePerformIO $ getAPIEntry "glutPostOverlayRedisplay"

-- glutPostRedisplay -----------------------------------------------------------

glutPostRedisplay :: MonadIO m => m ()
glutPostRedisplay = liftIO $ dyn_glutPostRedisplay ptr_glutPostRedisplay

foreign import CALLCONV "dynamic" dyn_glutPostRedisplay
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutPostRedisplay #-}
ptr_glutPostRedisplay :: FunPtr a
ptr_glutPostRedisplay = unsafePerformIO $ getAPIEntry "glutPostRedisplay"

-- glutPostWindowOverlayRedisplay ----------------------------------------------

glutPostWindowOverlayRedisplay :: MonadIO m => CInt -> m ()
glutPostWindowOverlayRedisplay v1 = liftIO $ dyn_glutPostWindowOverlayRedisplay ptr_glutPostWindowOverlayRedisplay v1

foreign import CALLCONV "dynamic" dyn_glutPostWindowOverlayRedisplay
  :: FunPtr (CInt -> IO ())
  ->         CInt -> IO ()

{-# NOINLINE ptr_glutPostWindowOverlayRedisplay #-}
ptr_glutPostWindowOverlayRedisplay :: FunPtr a
ptr_glutPostWindowOverlayRedisplay = unsafePerformIO $ getAPIEntry "glutPostWindowOverlayRedisplay"

-- glutPostWindowRedisplay -----------------------------------------------------

glutPostWindowRedisplay :: MonadIO m => CInt -> m ()
glutPostWindowRedisplay v1 = liftIO $ dyn_glutPostWindowRedisplay ptr_glutPostWindowRedisplay v1

foreign import CALLCONV "dynamic" dyn_glutPostWindowRedisplay
  :: FunPtr (CInt -> IO ())
  ->         CInt -> IO ()

{-# NOINLINE ptr_glutPostWindowRedisplay #-}
ptr_glutPostWindowRedisplay :: FunPtr a
ptr_glutPostWindowRedisplay = unsafePerformIO $ getAPIEntry "glutPostWindowRedisplay"

-- glutPushWindow --------------------------------------------------------------

glutPushWindow :: MonadIO m => m ()
glutPushWindow = liftIO $ dyn_glutPushWindow ptr_glutPushWindow

foreign import CALLCONV "dynamic" dyn_glutPushWindow
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutPushWindow #-}
ptr_glutPushWindow :: FunPtr a
ptr_glutPushWindow = unsafePerformIO $ getAPIEntry "glutPushWindow"

-- glutRemoveMenuItem ----------------------------------------------------------

glutRemoveMenuItem :: MonadIO m => CInt -> m ()
glutRemoveMenuItem v1 = liftIO $ dyn_glutRemoveMenuItem ptr_glutRemoveMenuItem v1

foreign import CALLCONV "dynamic" dyn_glutRemoveMenuItem
  :: FunPtr (CInt -> IO ())
  ->         CInt -> IO ()

{-# NOINLINE ptr_glutRemoveMenuItem #-}
ptr_glutRemoveMenuItem :: FunPtr a
ptr_glutRemoveMenuItem = unsafePerformIO $ getAPIEntry "glutRemoveMenuItem"

-- glutRemoveOverlay -----------------------------------------------------------

glutRemoveOverlay :: MonadIO m => m ()
glutRemoveOverlay = liftIO $ dyn_glutRemoveOverlay ptr_glutRemoveOverlay

foreign import CALLCONV "dynamic" dyn_glutRemoveOverlay
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutRemoveOverlay #-}
ptr_glutRemoveOverlay :: FunPtr a
ptr_glutRemoveOverlay = unsafePerformIO $ getAPIEntry "glutRemoveOverlay"

-- glutReportErrors ------------------------------------------------------------

glutReportErrors :: MonadIO m => m ()
glutReportErrors = liftIO $ dyn_glutReportErrors ptr_glutReportErrors

foreign import CALLCONV "dynamic" dyn_glutReportErrors
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutReportErrors #-}
ptr_glutReportErrors :: FunPtr a
ptr_glutReportErrors = unsafePerformIO $ getAPIEntry "glutReportErrors"

-- glutReshapeFunc -------------------------------------------------------------

glutReshapeFunc :: MonadIO m => FunPtr ReshapeFunc -> m ()
glutReshapeFunc v1 = liftIO $ dyn_glutReshapeFunc ptr_glutReshapeFunc v1

foreign import CALLCONV "dynamic" dyn_glutReshapeFunc
  :: FunPtr (FunPtr ReshapeFunc -> IO ())
  ->         FunPtr ReshapeFunc -> IO ()

{-# NOINLINE ptr_glutReshapeFunc #-}
ptr_glutReshapeFunc :: FunPtr a
ptr_glutReshapeFunc = unsafePerformIO $ getAPIEntry "glutReshapeFunc"

-- glutReshapeWindow -----------------------------------------------------------

glutReshapeWindow :: MonadIO m => CInt -> CInt -> m ()
glutReshapeWindow v1 v2 = liftIO $ dyn_glutReshapeWindow ptr_glutReshapeWindow v1 v2

foreign import CALLCONV "dynamic" dyn_glutReshapeWindow
  :: FunPtr (CInt -> CInt -> IO ())
  ->         CInt -> CInt -> IO ()

{-# NOINLINE ptr_glutReshapeWindow #-}
ptr_glutReshapeWindow :: FunPtr a
ptr_glutReshapeWindow = unsafePerformIO $ getAPIEntry "glutReshapeWindow"

-- glutSetColor ----------------------------------------------------------------

glutSetColor :: MonadIO m => CInt -> GLfloat -> GLfloat -> GLfloat -> m ()
glutSetColor v1 v2 v3 v4 = liftIO $ dyn_glutSetColor ptr_glutSetColor v1 v2 v3 v4

foreign import CALLCONV "dynamic" dyn_glutSetColor
  :: FunPtr (CInt -> GLfloat -> GLfloat -> GLfloat -> IO ())
  ->         CInt -> GLfloat -> GLfloat -> GLfloat -> IO ()

{-# NOINLINE ptr_glutSetColor #-}
ptr_glutSetColor :: FunPtr a
ptr_glutSetColor = unsafePerformIO $ getAPIEntry "glutSetColor"

-- glutSetCursor ---------------------------------------------------------------

glutSetCursor :: MonadIO m => CInt -> m ()
glutSetCursor v1 = liftIO $ dyn_glutSetCursor ptr_glutSetCursor v1

foreign import CALLCONV "dynamic" dyn_glutSetCursor
  :: FunPtr (CInt -> IO ())
  ->         CInt -> IO ()

{-# NOINLINE ptr_glutSetCursor #-}
ptr_glutSetCursor :: FunPtr a
ptr_glutSetCursor = unsafePerformIO $ getAPIEntry "glutSetCursor"

-- glutSetIconTitle ------------------------------------------------------------

glutSetIconTitle :: MonadIO m => Ptr CChar -> m ()
glutSetIconTitle v1 = liftIO $ dyn_glutSetIconTitle ptr_glutSetIconTitle v1

foreign import CALLCONV "dynamic" dyn_glutSetIconTitle
  :: FunPtr (Ptr CChar -> IO ())
  ->         Ptr CChar -> IO ()

{-# NOINLINE ptr_glutSetIconTitle #-}
ptr_glutSetIconTitle :: FunPtr a
ptr_glutSetIconTitle = unsafePerformIO $ getAPIEntry "glutSetIconTitle"

-- glutSetKeyRepeat ------------------------------------------------------------

glutSetKeyRepeat :: MonadIO m => CInt -> m ()
glutSetKeyRepeat v1 = liftIO $ dyn_glutSetKeyRepeat ptr_glutSetKeyRepeat v1

foreign import CALLCONV "dynamic" dyn_glutSetKeyRepeat
  :: FunPtr (CInt -> IO ())
  ->         CInt -> IO ()

{-# NOINLINE ptr_glutSetKeyRepeat #-}
ptr_glutSetKeyRepeat :: FunPtr a
ptr_glutSetKeyRepeat = unsafePerformIO $ getAPIEntry "glutSetKeyRepeat"

-- glutSetMenu -----------------------------------------------------------------

glutSetMenu :: MonadIO m => CInt -> m ()
glutSetMenu v1 = liftIO $ dyn_glutSetMenu ptr_glutSetMenu v1

foreign import CALLCONV "dynamic" dyn_glutSetMenu
  :: FunPtr (CInt -> IO ())
  ->         CInt -> IO ()

{-# NOINLINE ptr_glutSetMenu #-}
ptr_glutSetMenu :: FunPtr a
ptr_glutSetMenu = unsafePerformIO $ getAPIEntry "glutSetMenu"

-- glutSetMenuData -------------------------------------------------------------

glutSetMenuData :: MonadIO m => Ptr a -> m ()
glutSetMenuData v1 = liftIO $ dyn_glutSetMenuData ptr_glutSetMenuData v1

foreign import CALLCONV "dynamic" dyn_glutSetMenuData
  :: FunPtr (Ptr a -> IO ())
  ->         Ptr a -> IO ()

{-# NOINLINE ptr_glutSetMenuData #-}
ptr_glutSetMenuData :: FunPtr a
ptr_glutSetMenuData = unsafePerformIO $ getAPIEntry "glutSetMenuData"

-- glutSetMenuFont -------------------------------------------------------------

glutSetMenuFont :: MonadIO m => CInt -> Ptr a -> m ()
glutSetMenuFont v1 v2 = liftIO $ dyn_glutSetMenuFont ptr_glutSetMenuFont v1 v2

foreign import CALLCONV "dynamic" dyn_glutSetMenuFont
  :: FunPtr (CInt -> Ptr a -> IO ())
  ->         CInt -> Ptr a -> IO ()

{-# NOINLINE ptr_glutSetMenuFont #-}
ptr_glutSetMenuFont :: FunPtr a
ptr_glutSetMenuFont = unsafePerformIO $ getAPIEntry "glutSetMenuFont"

-- glutSetOption ---------------------------------------------------------------

glutSetOption :: MonadIO m => GLenum -> CInt -> m ()
glutSetOption v1 v2 = liftIO $ dyn_glutSetOption ptr_glutSetOption v1 v2

foreign import CALLCONV "dynamic" dyn_glutSetOption
  :: FunPtr (GLenum -> CInt -> IO ())
  ->         GLenum -> CInt -> IO ()

{-# NOINLINE ptr_glutSetOption #-}
ptr_glutSetOption :: FunPtr a
ptr_glutSetOption = unsafePerformIO $ getAPIEntry "glutSetOption"

-- glutSetVertexAttribCoord3 ---------------------------------------------------

glutSetVertexAttribCoord3 :: MonadIO m => GLint -> m ()
glutSetVertexAttribCoord3 v1 = liftIO $ dyn_glutSetVertexAttribCoord3 ptr_glutSetVertexAttribCoord3 v1

foreign import CALLCONV "dynamic" dyn_glutSetVertexAttribCoord3
  :: FunPtr (GLint -> IO ())
  ->         GLint -> IO ()

{-# NOINLINE ptr_glutSetVertexAttribCoord3 #-}
ptr_glutSetVertexAttribCoord3 :: FunPtr a
ptr_glutSetVertexAttribCoord3 = unsafePerformIO $ getAPIEntry "glutSetVertexAttribCoord3"

-- glutSetVertexAttribNormal ---------------------------------------------------

glutSetVertexAttribNormal :: MonadIO m => GLint -> m ()
glutSetVertexAttribNormal v1 = liftIO $ dyn_glutSetVertexAttribNormal ptr_glutSetVertexAttribNormal v1

foreign import CALLCONV "dynamic" dyn_glutSetVertexAttribNormal
  :: FunPtr (GLint -> IO ())
  ->         GLint -> IO ()

{-# NOINLINE ptr_glutSetVertexAttribNormal #-}
ptr_glutSetVertexAttribNormal :: FunPtr a
ptr_glutSetVertexAttribNormal = unsafePerformIO $ getAPIEntry "glutSetVertexAttribNormal"

-- glutSetVertexAttribTexCoord2 ------------------------------------------------

glutSetVertexAttribTexCoord2 :: MonadIO m => GLint -> m ()
glutSetVertexAttribTexCoord2 v1 = liftIO $ dyn_glutSetVertexAttribTexCoord2 ptr_glutSetVertexAttribTexCoord2 v1

foreign import CALLCONV "dynamic" dyn_glutSetVertexAttribTexCoord2
  :: FunPtr (GLint -> IO ())
  ->         GLint -> IO ()

{-# NOINLINE ptr_glutSetVertexAttribTexCoord2 #-}
ptr_glutSetVertexAttribTexCoord2 :: FunPtr a
ptr_glutSetVertexAttribTexCoord2 = unsafePerformIO $ getAPIEntry "glutSetVertexAttribTexCoord2"

-- glutSetWindow ---------------------------------------------------------------

glutSetWindow :: MonadIO m => CInt -> m ()
glutSetWindow v1 = liftIO $ dyn_glutSetWindow ptr_glutSetWindow v1

foreign import CALLCONV "dynamic" dyn_glutSetWindow
  :: FunPtr (CInt -> IO ())
  ->         CInt -> IO ()

{-# NOINLINE ptr_glutSetWindow #-}
ptr_glutSetWindow :: FunPtr a
ptr_glutSetWindow = unsafePerformIO $ getAPIEntry "glutSetWindow"

-- glutSetWindowData -----------------------------------------------------------

glutSetWindowData :: MonadIO m => Ptr a -> m ()
glutSetWindowData v1 = liftIO $ dyn_glutSetWindowData ptr_glutSetWindowData v1

foreign import CALLCONV "dynamic" dyn_glutSetWindowData
  :: FunPtr (Ptr a -> IO ())
  ->         Ptr a -> IO ()

{-# NOINLINE ptr_glutSetWindowData #-}
ptr_glutSetWindowData :: FunPtr a
ptr_glutSetWindowData = unsafePerformIO $ getAPIEntry "glutSetWindowData"

-- glutSetWindowTitle ----------------------------------------------------------

glutSetWindowTitle :: MonadIO m => Ptr CChar -> m ()
glutSetWindowTitle v1 = liftIO $ dyn_glutSetWindowTitle ptr_glutSetWindowTitle v1

foreign import CALLCONV "dynamic" dyn_glutSetWindowTitle
  :: FunPtr (Ptr CChar -> IO ())
  ->         Ptr CChar -> IO ()

{-# NOINLINE ptr_glutSetWindowTitle #-}
ptr_glutSetWindowTitle :: FunPtr a
ptr_glutSetWindowTitle = unsafePerformIO $ getAPIEntry "glutSetWindowTitle"

-- glutSetupVideoResizing ------------------------------------------------------

glutSetupVideoResizing :: MonadIO m => m ()
glutSetupVideoResizing = liftIO $ dyn_glutSetupVideoResizing ptr_glutSetupVideoResizing

foreign import CALLCONV "dynamic" dyn_glutSetupVideoResizing
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutSetupVideoResizing #-}
ptr_glutSetupVideoResizing :: FunPtr a
ptr_glutSetupVideoResizing = unsafePerformIO $ getAPIEntry "glutSetupVideoResizing"

-- glutShowOverlay -------------------------------------------------------------

glutShowOverlay :: MonadIO m => m ()
glutShowOverlay = liftIO $ dyn_glutShowOverlay ptr_glutShowOverlay

foreign import CALLCONV "dynamic" dyn_glutShowOverlay
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutShowOverlay #-}
ptr_glutShowOverlay :: FunPtr a
ptr_glutShowOverlay = unsafePerformIO $ getAPIEntry "glutShowOverlay"

-- glutShowWindow --------------------------------------------------------------

glutShowWindow :: MonadIO m => m ()
glutShowWindow = liftIO $ dyn_glutShowWindow ptr_glutShowWindow

foreign import CALLCONV "dynamic" dyn_glutShowWindow
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutShowWindow #-}
ptr_glutShowWindow :: FunPtr a
ptr_glutShowWindow = unsafePerformIO $ getAPIEntry "glutShowWindow"

-- glutSolidCone ---------------------------------------------------------------

glutSolidCone :: MonadIO m => GLdouble -> GLdouble -> GLint -> GLint -> m ()
glutSolidCone v1 v2 v3 v4 = liftIO $ dyn_glutSolidCone ptr_glutSolidCone v1 v2 v3 v4

foreign import CALLCONV "dynamic" dyn_glutSolidCone
  :: FunPtr (GLdouble -> GLdouble -> GLint -> GLint -> IO ())
  ->         GLdouble -> GLdouble -> GLint -> GLint -> IO ()

{-# NOINLINE ptr_glutSolidCone #-}
ptr_glutSolidCone :: FunPtr a
ptr_glutSolidCone = unsafePerformIO $ getAPIEntry "glutSolidCone"

-- glutSolidCube ---------------------------------------------------------------

glutSolidCube :: MonadIO m => GLdouble -> m ()
glutSolidCube v1 = liftIO $ dyn_glutSolidCube ptr_glutSolidCube v1

foreign import CALLCONV "dynamic" dyn_glutSolidCube
  :: FunPtr (GLdouble -> IO ())
  ->         GLdouble -> IO ()

{-# NOINLINE ptr_glutSolidCube #-}
ptr_glutSolidCube :: FunPtr a
ptr_glutSolidCube = unsafePerformIO $ getAPIEntry "glutSolidCube"

-- glutSolidCylinder -----------------------------------------------------------

glutSolidCylinder :: MonadIO m => GLdouble -> GLdouble -> GLint -> GLint -> m ()
glutSolidCylinder v1 v2 v3 v4 = liftIO $ dyn_glutSolidCylinder ptr_glutSolidCylinder v1 v2 v3 v4

foreign import CALLCONV "dynamic" dyn_glutSolidCylinder
  :: FunPtr (GLdouble -> GLdouble -> GLint -> GLint -> IO ())
  ->         GLdouble -> GLdouble -> GLint -> GLint -> IO ()

{-# NOINLINE ptr_glutSolidCylinder #-}
ptr_glutSolidCylinder :: FunPtr a
ptr_glutSolidCylinder = unsafePerformIO $ getAPIEntry "glutSolidCylinder"

-- glutSolidDodecahedron -------------------------------------------------------

glutSolidDodecahedron :: MonadIO m => m ()
glutSolidDodecahedron = liftIO $ dyn_glutSolidDodecahedron ptr_glutSolidDodecahedron

foreign import CALLCONV "dynamic" dyn_glutSolidDodecahedron
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutSolidDodecahedron #-}
ptr_glutSolidDodecahedron :: FunPtr a
ptr_glutSolidDodecahedron = unsafePerformIO $ getAPIEntry "glutSolidDodecahedron"

-- glutSolidIcosahedron --------------------------------------------------------

glutSolidIcosahedron :: MonadIO m => m ()
glutSolidIcosahedron = liftIO $ dyn_glutSolidIcosahedron ptr_glutSolidIcosahedron

foreign import CALLCONV "dynamic" dyn_glutSolidIcosahedron
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutSolidIcosahedron #-}
ptr_glutSolidIcosahedron :: FunPtr a
ptr_glutSolidIcosahedron = unsafePerformIO $ getAPIEntry "glutSolidIcosahedron"

-- glutSolidOctahedron ---------------------------------------------------------

glutSolidOctahedron :: MonadIO m => m ()
glutSolidOctahedron = liftIO $ dyn_glutSolidOctahedron ptr_glutSolidOctahedron

foreign import CALLCONV "dynamic" dyn_glutSolidOctahedron
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutSolidOctahedron #-}
ptr_glutSolidOctahedron :: FunPtr a
ptr_glutSolidOctahedron = unsafePerformIO $ getAPIEntry "glutSolidOctahedron"

-- glutSolidRhombicDodecahedron ------------------------------------------------

glutSolidRhombicDodecahedron :: MonadIO m => m ()
glutSolidRhombicDodecahedron = liftIO $ dyn_glutSolidRhombicDodecahedron ptr_glutSolidRhombicDodecahedron

foreign import CALLCONV "dynamic" dyn_glutSolidRhombicDodecahedron
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutSolidRhombicDodecahedron #-}
ptr_glutSolidRhombicDodecahedron :: FunPtr a
ptr_glutSolidRhombicDodecahedron = unsafePerformIO $ getAPIEntry "glutSolidRhombicDodecahedron"

-- glutSolidSierpinskiSponge ---------------------------------------------------

glutSolidSierpinskiSponge :: MonadIO m => CInt -> Ptr GLdouble -> GLdouble -> m ()
glutSolidSierpinskiSponge v1 v2 v3 = liftIO $ dyn_glutSolidSierpinskiSponge ptr_glutSolidSierpinskiSponge v1 v2 v3

foreign import CALLCONV "dynamic" dyn_glutSolidSierpinskiSponge
  :: FunPtr (CInt -> Ptr GLdouble -> GLdouble -> IO ())
  ->         CInt -> Ptr GLdouble -> GLdouble -> IO ()

{-# NOINLINE ptr_glutSolidSierpinskiSponge #-}
ptr_glutSolidSierpinskiSponge :: FunPtr a
ptr_glutSolidSierpinskiSponge = unsafePerformIO $ getAPIEntry "glutSolidSierpinskiSponge"

-- glutSolidSphere -------------------------------------------------------------

glutSolidSphere :: MonadIO m => GLdouble -> GLint -> GLint -> m ()
glutSolidSphere v1 v2 v3 = liftIO $ dyn_glutSolidSphere ptr_glutSolidSphere v1 v2 v3

foreign import CALLCONV "dynamic" dyn_glutSolidSphere
  :: FunPtr (GLdouble -> GLint -> GLint -> IO ())
  ->         GLdouble -> GLint -> GLint -> IO ()

{-# NOINLINE ptr_glutSolidSphere #-}
ptr_glutSolidSphere :: FunPtr a
ptr_glutSolidSphere = unsafePerformIO $ getAPIEntry "glutSolidSphere"

-- glutSolidTeacup -------------------------------------------------------------

glutSolidTeacup :: MonadIO m => GLdouble -> m ()
glutSolidTeacup v1 = liftIO $ dyn_glutSolidTeacup ptr_glutSolidTeacup v1

foreign import CALLCONV "dynamic" dyn_glutSolidTeacup
  :: FunPtr (GLdouble -> IO ())
  ->         GLdouble -> IO ()

{-# NOINLINE ptr_glutSolidTeacup #-}
ptr_glutSolidTeacup :: FunPtr a
ptr_glutSolidTeacup = unsafePerformIO $ getAPIEntry "glutSolidTeacup"

-- glutSolidTeapot -------------------------------------------------------------

glutSolidTeapot :: MonadIO m => GLdouble -> m ()
glutSolidTeapot v1 = liftIO $ dyn_glutSolidTeapot ptr_glutSolidTeapot v1

foreign import CALLCONV "dynamic" dyn_glutSolidTeapot
  :: FunPtr (GLdouble -> IO ())
  ->         GLdouble -> IO ()

{-# NOINLINE ptr_glutSolidTeapot #-}
ptr_glutSolidTeapot :: FunPtr a
ptr_glutSolidTeapot = unsafePerformIO $ getAPIEntry "glutSolidTeapot"

-- glutSolidTeaspoon -----------------------------------------------------------

glutSolidTeaspoon :: MonadIO m => GLdouble -> m ()
glutSolidTeaspoon v1 = liftIO $ dyn_glutSolidTeaspoon ptr_glutSolidTeaspoon v1

foreign import CALLCONV "dynamic" dyn_glutSolidTeaspoon
  :: FunPtr (GLdouble -> IO ())
  ->         GLdouble -> IO ()

{-# NOINLINE ptr_glutSolidTeaspoon #-}
ptr_glutSolidTeaspoon :: FunPtr a
ptr_glutSolidTeaspoon = unsafePerformIO $ getAPIEntry "glutSolidTeaspoon"

-- glutSolidTetrahedron --------------------------------------------------------

glutSolidTetrahedron :: MonadIO m => m ()
glutSolidTetrahedron = liftIO $ dyn_glutSolidTetrahedron ptr_glutSolidTetrahedron

foreign import CALLCONV "dynamic" dyn_glutSolidTetrahedron
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutSolidTetrahedron #-}
ptr_glutSolidTetrahedron :: FunPtr a
ptr_glutSolidTetrahedron = unsafePerformIO $ getAPIEntry "glutSolidTetrahedron"

-- glutSolidTorus --------------------------------------------------------------

glutSolidTorus :: MonadIO m => GLdouble -> GLdouble -> GLint -> GLint -> m ()
glutSolidTorus v1 v2 v3 v4 = liftIO $ dyn_glutSolidTorus ptr_glutSolidTorus v1 v2 v3 v4

foreign import CALLCONV "dynamic" dyn_glutSolidTorus
  :: FunPtr (GLdouble -> GLdouble -> GLint -> GLint -> IO ())
  ->         GLdouble -> GLdouble -> GLint -> GLint -> IO ()

{-# NOINLINE ptr_glutSolidTorus #-}
ptr_glutSolidTorus :: FunPtr a
ptr_glutSolidTorus = unsafePerformIO $ getAPIEntry "glutSolidTorus"

-- glutSpaceballButtonFunc -----------------------------------------------------

glutSpaceballButtonFunc :: MonadIO m => FunPtr SpaceballButtonFunc -> m ()
glutSpaceballButtonFunc v1 = liftIO $ dyn_glutSpaceballButtonFunc ptr_glutSpaceballButtonFunc v1

foreign import CALLCONV "dynamic" dyn_glutSpaceballButtonFunc
  :: FunPtr (FunPtr SpaceballButtonFunc -> IO ())
  ->         FunPtr SpaceballButtonFunc -> IO ()

{-# NOINLINE ptr_glutSpaceballButtonFunc #-}
ptr_glutSpaceballButtonFunc :: FunPtr a
ptr_glutSpaceballButtonFunc = unsafePerformIO $ getAPIEntry "glutSpaceballButtonFunc"

-- glutSpaceballMotionFunc -----------------------------------------------------

glutSpaceballMotionFunc :: MonadIO m => FunPtr SpaceballMotionFunc -> m ()
glutSpaceballMotionFunc v1 = liftIO $ dyn_glutSpaceballMotionFunc ptr_glutSpaceballMotionFunc v1

foreign import CALLCONV "dynamic" dyn_glutSpaceballMotionFunc
  :: FunPtr (FunPtr SpaceballMotionFunc -> IO ())
  ->         FunPtr SpaceballMotionFunc -> IO ()

{-# NOINLINE ptr_glutSpaceballMotionFunc #-}
ptr_glutSpaceballMotionFunc :: FunPtr a
ptr_glutSpaceballMotionFunc = unsafePerformIO $ getAPIEntry "glutSpaceballMotionFunc"

-- glutSpaceballRotateFunc -----------------------------------------------------

glutSpaceballRotateFunc :: MonadIO m => FunPtr SpaceballRotateFunc -> m ()
glutSpaceballRotateFunc v1 = liftIO $ dyn_glutSpaceballRotateFunc ptr_glutSpaceballRotateFunc v1

foreign import CALLCONV "dynamic" dyn_glutSpaceballRotateFunc
  :: FunPtr (FunPtr SpaceballRotateFunc -> IO ())
  ->         FunPtr SpaceballRotateFunc -> IO ()

{-# NOINLINE ptr_glutSpaceballRotateFunc #-}
ptr_glutSpaceballRotateFunc :: FunPtr a
ptr_glutSpaceballRotateFunc = unsafePerformIO $ getAPIEntry "glutSpaceballRotateFunc"

-- glutSpecialFunc -------------------------------------------------------------

glutSpecialFunc :: MonadIO m => FunPtr SpecialFunc -> m ()
glutSpecialFunc v1 = liftIO $ dyn_glutSpecialFunc ptr_glutSpecialFunc v1

foreign import CALLCONV "dynamic" dyn_glutSpecialFunc
  :: FunPtr (FunPtr SpecialFunc -> IO ())
  ->         FunPtr SpecialFunc -> IO ()

{-# NOINLINE ptr_glutSpecialFunc #-}
ptr_glutSpecialFunc :: FunPtr a
ptr_glutSpecialFunc = unsafePerformIO $ getAPIEntry "glutSpecialFunc"

-- glutSpecialUpFunc -----------------------------------------------------------

glutSpecialUpFunc :: MonadIO m => FunPtr SpecialUpFunc -> m ()
glutSpecialUpFunc v1 = liftIO $ dyn_glutSpecialUpFunc ptr_glutSpecialUpFunc v1

foreign import CALLCONV "dynamic" dyn_glutSpecialUpFunc
  :: FunPtr (FunPtr SpecialUpFunc -> IO ())
  ->         FunPtr SpecialUpFunc -> IO ()

{-# NOINLINE ptr_glutSpecialUpFunc #-}
ptr_glutSpecialUpFunc :: FunPtr a
ptr_glutSpecialUpFunc = unsafePerformIO $ getAPIEntry "glutSpecialUpFunc"

-- glutStopVideoResizing -------------------------------------------------------

glutStopVideoResizing :: MonadIO m => m ()
glutStopVideoResizing = liftIO $ dyn_glutStopVideoResizing ptr_glutStopVideoResizing

foreign import CALLCONV "dynamic" dyn_glutStopVideoResizing
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutStopVideoResizing #-}
ptr_glutStopVideoResizing :: FunPtr a
ptr_glutStopVideoResizing = unsafePerformIO $ getAPIEntry "glutStopVideoResizing"

-- glutStrokeCharacter ---------------------------------------------------------

glutStrokeCharacter :: MonadIO m => Ptr a -> CInt -> m ()
glutStrokeCharacter v1 v2 = liftIO $ dyn_glutStrokeCharacter ptr_glutStrokeCharacter v1 v2

foreign import CALLCONV "dynamic" dyn_glutStrokeCharacter
  :: FunPtr (Ptr a -> CInt -> IO ())
  ->         Ptr a -> CInt -> IO ()

{-# NOINLINE ptr_glutStrokeCharacter #-}
ptr_glutStrokeCharacter :: FunPtr a
ptr_glutStrokeCharacter = unsafePerformIO $ getAPIEntry "glutStrokeCharacter"

-- glutStrokeHeight ------------------------------------------------------------

glutStrokeHeight :: MonadIO m => Ptr a -> m GLfloat
glutStrokeHeight v1 = liftIO $ dyn_glutStrokeHeight ptr_glutStrokeHeight v1

foreign import CALLCONV "dynamic" dyn_glutStrokeHeight
  :: FunPtr (Ptr a -> IO GLfloat)
  ->         Ptr a -> IO GLfloat

{-# NOINLINE ptr_glutStrokeHeight #-}
ptr_glutStrokeHeight :: FunPtr a
ptr_glutStrokeHeight = unsafePerformIO $ getAPIEntry "glutStrokeHeight"

-- glutStrokeLength ------------------------------------------------------------

glutStrokeLength :: MonadIO m => Ptr a -> Ptr CUChar -> m CInt
glutStrokeLength v1 v2 = liftIO $ dyn_glutStrokeLength ptr_glutStrokeLength v1 v2

foreign import CALLCONV "dynamic" dyn_glutStrokeLength
  :: FunPtr (Ptr a -> Ptr CUChar -> IO CInt)
  ->         Ptr a -> Ptr CUChar -> IO CInt

{-# NOINLINE ptr_glutStrokeLength #-}
ptr_glutStrokeLength :: FunPtr a
ptr_glutStrokeLength = unsafePerformIO $ getAPIEntry "glutStrokeLength"

-- glutStrokeString ------------------------------------------------------------

glutStrokeString :: MonadIO m => Ptr a -> Ptr CUChar -> m ()
glutStrokeString v1 v2 = liftIO $ dyn_glutStrokeString ptr_glutStrokeString v1 v2

foreign import CALLCONV "dynamic" dyn_glutStrokeString
  :: FunPtr (Ptr a -> Ptr CUChar -> IO ())
  ->         Ptr a -> Ptr CUChar -> IO ()

{-# NOINLINE ptr_glutStrokeString #-}
ptr_glutStrokeString :: FunPtr a
ptr_glutStrokeString = unsafePerformIO $ getAPIEntry "glutStrokeString"

-- glutStrokeWidth -------------------------------------------------------------

glutStrokeWidth :: MonadIO m => Ptr a -> CInt -> m CInt
glutStrokeWidth v1 v2 = liftIO $ dyn_glutStrokeWidth ptr_glutStrokeWidth v1 v2

foreign import CALLCONV "dynamic" dyn_glutStrokeWidth
  :: FunPtr (Ptr a -> CInt -> IO CInt)
  ->         Ptr a -> CInt -> IO CInt

{-# NOINLINE ptr_glutStrokeWidth #-}
ptr_glutStrokeWidth :: FunPtr a
ptr_glutStrokeWidth = unsafePerformIO $ getAPIEntry "glutStrokeWidth"

-- glutSwapBuffers -------------------------------------------------------------

glutSwapBuffers :: MonadIO m => m ()
glutSwapBuffers = liftIO $ dyn_glutSwapBuffers ptr_glutSwapBuffers

foreign import CALLCONV "dynamic" dyn_glutSwapBuffers
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutSwapBuffers #-}
ptr_glutSwapBuffers :: FunPtr a
ptr_glutSwapBuffers = unsafePerformIO $ getAPIEntry "glutSwapBuffers"

-- glutTabletButtonFunc --------------------------------------------------------

glutTabletButtonFunc :: MonadIO m => FunPtr TabletButtonFunc -> m ()
glutTabletButtonFunc v1 = liftIO $ dyn_glutTabletButtonFunc ptr_glutTabletButtonFunc v1

foreign import CALLCONV "dynamic" dyn_glutTabletButtonFunc
  :: FunPtr (FunPtr TabletButtonFunc -> IO ())
  ->         FunPtr TabletButtonFunc -> IO ()

{-# NOINLINE ptr_glutTabletButtonFunc #-}
ptr_glutTabletButtonFunc :: FunPtr a
ptr_glutTabletButtonFunc = unsafePerformIO $ getAPIEntry "glutTabletButtonFunc"

-- glutTabletMotionFunc --------------------------------------------------------

glutTabletMotionFunc :: MonadIO m => FunPtr TabletMotionFunc -> m ()
glutTabletMotionFunc v1 = liftIO $ dyn_glutTabletMotionFunc ptr_glutTabletMotionFunc v1

foreign import CALLCONV "dynamic" dyn_glutTabletMotionFunc
  :: FunPtr (FunPtr TabletMotionFunc -> IO ())
  ->         FunPtr TabletMotionFunc -> IO ()

{-# NOINLINE ptr_glutTabletMotionFunc #-}
ptr_glutTabletMotionFunc :: FunPtr a
ptr_glutTabletMotionFunc = unsafePerformIO $ getAPIEntry "glutTabletMotionFunc"

-- glutTimerFunc ---------------------------------------------------------------

glutTimerFunc :: MonadIO m => CUInt -> FunPtr TimerFunc -> CInt -> m ()
glutTimerFunc v1 v2 v3 = liftIO $ dyn_glutTimerFunc ptr_glutTimerFunc v1 v2 v3

foreign import CALLCONV "dynamic" dyn_glutTimerFunc
  :: FunPtr (CUInt -> FunPtr TimerFunc -> CInt -> IO ())
  ->         CUInt -> FunPtr TimerFunc -> CInt -> IO ()

{-# NOINLINE ptr_glutTimerFunc #-}
ptr_glutTimerFunc :: FunPtr a
ptr_glutTimerFunc = unsafePerformIO $ getAPIEntry "glutTimerFunc"

-- glutUseLayer ----------------------------------------------------------------

glutUseLayer :: MonadIO m => GLenum -> m ()
glutUseLayer v1 = liftIO $ dyn_glutUseLayer ptr_glutUseLayer v1

foreign import CALLCONV "dynamic" dyn_glutUseLayer
  :: FunPtr (GLenum -> IO ())
  ->         GLenum -> IO ()

{-# NOINLINE ptr_glutUseLayer #-}
ptr_glutUseLayer :: FunPtr a
ptr_glutUseLayer = unsafePerformIO $ getAPIEntry "glutUseLayer"

-- glutVideoPan ----------------------------------------------------------------

glutVideoPan :: MonadIO m => CInt -> CInt -> CInt -> CInt -> m ()
glutVideoPan v1 v2 v3 v4 = liftIO $ dyn_glutVideoPan ptr_glutVideoPan v1 v2 v3 v4

foreign import CALLCONV "dynamic" dyn_glutVideoPan
  :: FunPtr (CInt -> CInt -> CInt -> CInt -> IO ())
  ->         CInt -> CInt -> CInt -> CInt -> IO ()

{-# NOINLINE ptr_glutVideoPan #-}
ptr_glutVideoPan :: FunPtr a
ptr_glutVideoPan = unsafePerformIO $ getAPIEntry "glutVideoPan"

-- glutVideoResize -------------------------------------------------------------

glutVideoResize :: MonadIO m => CInt -> CInt -> CInt -> CInt -> m ()
glutVideoResize v1 v2 v3 v4 = liftIO $ dyn_glutVideoResize ptr_glutVideoResize v1 v2 v3 v4

foreign import CALLCONV "dynamic" dyn_glutVideoResize
  :: FunPtr (CInt -> CInt -> CInt -> CInt -> IO ())
  ->         CInt -> CInt -> CInt -> CInt -> IO ()

{-# NOINLINE ptr_glutVideoResize #-}
ptr_glutVideoResize :: FunPtr a
ptr_glutVideoResize = unsafePerformIO $ getAPIEntry "glutVideoResize"

-- glutVideoResizeGet ----------------------------------------------------------

glutVideoResizeGet :: MonadIO m => GLenum -> m CInt
glutVideoResizeGet v1 = liftIO $ dyn_glutVideoResizeGet ptr_glutVideoResizeGet v1

foreign import CALLCONV "dynamic" dyn_glutVideoResizeGet
  :: FunPtr (GLenum -> IO CInt)
  ->         GLenum -> IO CInt

{-# NOINLINE ptr_glutVideoResizeGet #-}
ptr_glutVideoResizeGet :: FunPtr a
ptr_glutVideoResizeGet = unsafePerformIO $ getAPIEntry "glutVideoResizeGet"

-- glutVisibilityFunc ----------------------------------------------------------

glutVisibilityFunc :: MonadIO m => FunPtr VisibilityFunc -> m ()
glutVisibilityFunc v1 = liftIO $ dyn_glutVisibilityFunc ptr_glutVisibilityFunc v1

foreign import CALLCONV "dynamic" dyn_glutVisibilityFunc
  :: FunPtr (FunPtr VisibilityFunc -> IO ())
  ->         FunPtr VisibilityFunc -> IO ()

{-# NOINLINE ptr_glutVisibilityFunc #-}
ptr_glutVisibilityFunc :: FunPtr a
ptr_glutVisibilityFunc = unsafePerformIO $ getAPIEntry "glutVisibilityFunc"

-- glutWMCloseFunc -------------------------------------------------------------

glutWMCloseFunc :: MonadIO m => FunPtr WMCloseFunc -> m ()
glutWMCloseFunc v1 = liftIO $ dyn_glutWMCloseFunc ptr_glutWMCloseFunc v1

foreign import CALLCONV "dynamic" dyn_glutWMCloseFunc
  :: FunPtr (FunPtr WMCloseFunc -> IO ())
  ->         FunPtr WMCloseFunc -> IO ()

{-# NOINLINE ptr_glutWMCloseFunc #-}
ptr_glutWMCloseFunc :: FunPtr a
ptr_glutWMCloseFunc = unsafePerformIO $ getAPIEntry "glutWMCloseFunc"

-- glutWarpPointer -------------------------------------------------------------

glutWarpPointer :: MonadIO m => CInt -> CInt -> m ()
glutWarpPointer v1 v2 = liftIO $ dyn_glutWarpPointer ptr_glutWarpPointer v1 v2

foreign import CALLCONV "dynamic" dyn_glutWarpPointer
  :: FunPtr (CInt -> CInt -> IO ())
  ->         CInt -> CInt -> IO ()

{-# NOINLINE ptr_glutWarpPointer #-}
ptr_glutWarpPointer :: FunPtr a
ptr_glutWarpPointer = unsafePerformIO $ getAPIEntry "glutWarpPointer"

-- glutWindowStatusFunc --------------------------------------------------------

glutWindowStatusFunc :: MonadIO m => FunPtr WindowStatusFunc -> m ()
glutWindowStatusFunc v1 = liftIO $ dyn_glutWindowStatusFunc ptr_glutWindowStatusFunc v1

foreign import CALLCONV "dynamic" dyn_glutWindowStatusFunc
  :: FunPtr (FunPtr WindowStatusFunc -> IO ())
  ->         FunPtr WindowStatusFunc -> IO ()

{-# NOINLINE ptr_glutWindowStatusFunc #-}
ptr_glutWindowStatusFunc :: FunPtr a
ptr_glutWindowStatusFunc = unsafePerformIO $ getAPIEntry "glutWindowStatusFunc"

-- glutWireCone ----------------------------------------------------------------

glutWireCone :: MonadIO m => GLdouble -> GLdouble -> GLint -> GLint -> m ()
glutWireCone v1 v2 v3 v4 = liftIO $ dyn_glutWireCone ptr_glutWireCone v1 v2 v3 v4

foreign import CALLCONV "dynamic" dyn_glutWireCone
  :: FunPtr (GLdouble -> GLdouble -> GLint -> GLint -> IO ())
  ->         GLdouble -> GLdouble -> GLint -> GLint -> IO ()

{-# NOINLINE ptr_glutWireCone #-}
ptr_glutWireCone :: FunPtr a
ptr_glutWireCone = unsafePerformIO $ getAPIEntry "glutWireCone"

-- glutWireCube ----------------------------------------------------------------

glutWireCube :: MonadIO m => GLdouble -> m ()
glutWireCube v1 = liftIO $ dyn_glutWireCube ptr_glutWireCube v1

foreign import CALLCONV "dynamic" dyn_glutWireCube
  :: FunPtr (GLdouble -> IO ())
  ->         GLdouble -> IO ()

{-# NOINLINE ptr_glutWireCube #-}
ptr_glutWireCube :: FunPtr a
ptr_glutWireCube = unsafePerformIO $ getAPIEntry "glutWireCube"

-- glutWireCylinder ------------------------------------------------------------

glutWireCylinder :: MonadIO m => GLdouble -> GLdouble -> GLint -> GLint -> m ()
glutWireCylinder v1 v2 v3 v4 = liftIO $ dyn_glutWireCylinder ptr_glutWireCylinder v1 v2 v3 v4

foreign import CALLCONV "dynamic" dyn_glutWireCylinder
  :: FunPtr (GLdouble -> GLdouble -> GLint -> GLint -> IO ())
  ->         GLdouble -> GLdouble -> GLint -> GLint -> IO ()

{-# NOINLINE ptr_glutWireCylinder #-}
ptr_glutWireCylinder :: FunPtr a
ptr_glutWireCylinder = unsafePerformIO $ getAPIEntry "glutWireCylinder"

-- glutWireDodecahedron --------------------------------------------------------

glutWireDodecahedron :: MonadIO m => m ()
glutWireDodecahedron = liftIO $ dyn_glutWireDodecahedron ptr_glutWireDodecahedron

foreign import CALLCONV "dynamic" dyn_glutWireDodecahedron
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutWireDodecahedron #-}
ptr_glutWireDodecahedron :: FunPtr a
ptr_glutWireDodecahedron = unsafePerformIO $ getAPIEntry "glutWireDodecahedron"

-- glutWireIcosahedron ---------------------------------------------------------

glutWireIcosahedron :: MonadIO m => m ()
glutWireIcosahedron = liftIO $ dyn_glutWireIcosahedron ptr_glutWireIcosahedron

foreign import CALLCONV "dynamic" dyn_glutWireIcosahedron
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutWireIcosahedron #-}
ptr_glutWireIcosahedron :: FunPtr a
ptr_glutWireIcosahedron = unsafePerformIO $ getAPIEntry "glutWireIcosahedron"

-- glutWireOctahedron ----------------------------------------------------------

glutWireOctahedron :: MonadIO m => m ()
glutWireOctahedron = liftIO $ dyn_glutWireOctahedron ptr_glutWireOctahedron

foreign import CALLCONV "dynamic" dyn_glutWireOctahedron
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutWireOctahedron #-}
ptr_glutWireOctahedron :: FunPtr a
ptr_glutWireOctahedron = unsafePerformIO $ getAPIEntry "glutWireOctahedron"

-- glutWireRhombicDodecahedron -------------------------------------------------

glutWireRhombicDodecahedron :: MonadIO m => m ()
glutWireRhombicDodecahedron = liftIO $ dyn_glutWireRhombicDodecahedron ptr_glutWireRhombicDodecahedron

foreign import CALLCONV "dynamic" dyn_glutWireRhombicDodecahedron
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutWireRhombicDodecahedron #-}
ptr_glutWireRhombicDodecahedron :: FunPtr a
ptr_glutWireRhombicDodecahedron = unsafePerformIO $ getAPIEntry "glutWireRhombicDodecahedron"

-- glutWireSierpinskiSponge ----------------------------------------------------

glutWireSierpinskiSponge :: MonadIO m => CInt -> Ptr GLdouble -> GLdouble -> m ()
glutWireSierpinskiSponge v1 v2 v3 = liftIO $ dyn_glutWireSierpinskiSponge ptr_glutWireSierpinskiSponge v1 v2 v3

foreign import CALLCONV "dynamic" dyn_glutWireSierpinskiSponge
  :: FunPtr (CInt -> Ptr GLdouble -> GLdouble -> IO ())
  ->         CInt -> Ptr GLdouble -> GLdouble -> IO ()

{-# NOINLINE ptr_glutWireSierpinskiSponge #-}
ptr_glutWireSierpinskiSponge :: FunPtr a
ptr_glutWireSierpinskiSponge = unsafePerformIO $ getAPIEntry "glutWireSierpinskiSponge"

-- glutWireSphere --------------------------------------------------------------

glutWireSphere :: MonadIO m => GLdouble -> GLint -> GLint -> m ()
glutWireSphere v1 v2 v3 = liftIO $ dyn_glutWireSphere ptr_glutWireSphere v1 v2 v3

foreign import CALLCONV "dynamic" dyn_glutWireSphere
  :: FunPtr (GLdouble -> GLint -> GLint -> IO ())
  ->         GLdouble -> GLint -> GLint -> IO ()

{-# NOINLINE ptr_glutWireSphere #-}
ptr_glutWireSphere :: FunPtr a
ptr_glutWireSphere = unsafePerformIO $ getAPIEntry "glutWireSphere"

-- glutWireTeacup --------------------------------------------------------------

glutWireTeacup :: MonadIO m => GLdouble -> m ()
glutWireTeacup v1 = liftIO $ dyn_glutWireTeacup ptr_glutWireTeacup v1

foreign import CALLCONV "dynamic" dyn_glutWireTeacup
  :: FunPtr (GLdouble -> IO ())
  ->         GLdouble -> IO ()

{-# NOINLINE ptr_glutWireTeacup #-}
ptr_glutWireTeacup :: FunPtr a
ptr_glutWireTeacup = unsafePerformIO $ getAPIEntry "glutWireTeacup"

-- glutWireTeapot --------------------------------------------------------------

glutWireTeapot :: MonadIO m => GLdouble -> m ()
glutWireTeapot v1 = liftIO $ dyn_glutWireTeapot ptr_glutWireTeapot v1

foreign import CALLCONV "dynamic" dyn_glutWireTeapot
  :: FunPtr (GLdouble -> IO ())
  ->         GLdouble -> IO ()

{-# NOINLINE ptr_glutWireTeapot #-}
ptr_glutWireTeapot :: FunPtr a
ptr_glutWireTeapot = unsafePerformIO $ getAPIEntry "glutWireTeapot"

-- glutWireTeaspoon ------------------------------------------------------------

glutWireTeaspoon :: MonadIO m => GLdouble -> m ()
glutWireTeaspoon v1 = liftIO $ dyn_glutWireTeaspoon ptr_glutWireTeaspoon v1

foreign import CALLCONV "dynamic" dyn_glutWireTeaspoon
  :: FunPtr (GLdouble -> IO ())
  ->         GLdouble -> IO ()

{-# NOINLINE ptr_glutWireTeaspoon #-}
ptr_glutWireTeaspoon :: FunPtr a
ptr_glutWireTeaspoon = unsafePerformIO $ getAPIEntry "glutWireTeaspoon"

-- glutWireTetrahedron ---------------------------------------------------------

glutWireTetrahedron :: MonadIO m => m ()
glutWireTetrahedron = liftIO $ dyn_glutWireTetrahedron ptr_glutWireTetrahedron

foreign import CALLCONV "dynamic" dyn_glutWireTetrahedron
  :: FunPtr (IO ())
  ->         IO ()

{-# NOINLINE ptr_glutWireTetrahedron #-}
ptr_glutWireTetrahedron :: FunPtr a
ptr_glutWireTetrahedron = unsafePerformIO $ getAPIEntry "glutWireTetrahedron"

-- glutWireTorus ---------------------------------------------------------------

glutWireTorus :: MonadIO m => GLdouble -> GLdouble -> GLint -> GLint -> m ()
glutWireTorus v1 v2 v3 v4 = liftIO $ dyn_glutWireTorus ptr_glutWireTorus v1 v2 v3 v4

foreign import CALLCONV "dynamic" dyn_glutWireTorus
  :: FunPtr (GLdouble -> GLdouble -> GLint -> GLint -> IO ())
  ->         GLdouble -> GLdouble -> GLint -> GLint -> IO ()

{-# NOINLINE ptr_glutWireTorus #-}
ptr_glutWireTorus :: FunPtr a
ptr_glutWireTorus = unsafePerformIO $ getAPIEntry "glutWireTorus"

