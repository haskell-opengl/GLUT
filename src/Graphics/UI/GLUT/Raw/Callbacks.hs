{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Raw.Callbacks
-- Copyright   :  (c) Sven Panne 2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- All GLUT callbacks.
--
-----------------------------------------------------------------------------

module Graphics.UI.GLUT.Raw.Callbacks (
   AppStatusFunc, makeAppStatusFunc,
   ButtonBoxFunc, makeButtonBoxFunc,
   CloseFunc, makeCloseFunc,
   DialsFunc, makeDialsFunc,
   DisplayFunc, makeDisplayFunc,
   EntryFunc, makeEntryFunc,
   IdleFunc, makeIdleFunc,
   InitContextFunc, makeInitContextFunc,
   JoystickFunc, makeJoystickFunc,
   KeyboardFunc, makeKeyboardFunc,
   KeyboardUpFunc, makeKeyboardUpFunc,
   MenuDestroyFunc, makeMenuDestroyFunc,
   MenuFunc, makeMenuFunc,
   MenuStateFunc, makeMenuStateFunc,
   MenuStatusFunc, makeMenuStatusFunc,
   MotionFunc, makeMotionFunc,
   MouseFunc, makeMouseFunc,
   MouseWheelFunc, makeMouseWheelFunc,
   MultiButtonFunc, makeMultiButtonFunc,
   MultiEntryFunc, makeMultiEntryFunc,
   MultiMotionFunc, makeMultiMotionFunc,
   MultiPassiveFunc, makeMultiPassiveFunc,
   OverlayDisplayFunc, makeOverlayDisplayFunc,
   PassiveMotionFunc, makePassiveMotionFunc,
   PositionFunc, makePositionFunc,
   ReshapeFunc, makeReshapeFunc,
   SpaceballButtonFunc, makeSpaceballButtonFunc,
   SpaceballMotionFunc, makeSpaceballMotionFunc,
   SpaceballRotateFunc, makeSpaceballRotateFunc,
   SpecialFunc, makeSpecialFunc,
   SpecialUpFunc, makeSpecialUpFunc,
   TabletButtonFunc, makeTabletButtonFunc,
   TabletMotionFunc, makeTabletMotionFunc,
   TimerFunc, makeTimerFunc,
   VisibilityFunc, makeVisibilityFunc,
   WMCloseFunc, makeWMCloseFunc,
   WindowStatusFunc, makeWindowStatusFunc
) where

import Foreign.C.Types
import Foreign.Ptr

type AppStatusFunc = CInt -> IO ()

foreign import ccall "wrapper"
   makeAppStatusFunc :: AppStatusFunc -> IO (FunPtr AppStatusFunc)

type ButtonBoxFunc = CInt -> CInt -> IO ()

foreign import ccall "wrapper"
   makeButtonBoxFunc :: ButtonBoxFunc -> IO (FunPtr ButtonBoxFunc)

type CloseFunc = IO ()

foreign import ccall "wrapper"
   makeCloseFunc :: CloseFunc -> IO (FunPtr CloseFunc)

type DialsFunc = CInt -> CInt -> IO ()

foreign import ccall "wrapper"
   makeDialsFunc :: DialsFunc -> IO (FunPtr DialsFunc)

type DisplayFunc = IO ()

foreign import ccall "wrapper"
   makeDisplayFunc :: DisplayFunc -> IO (FunPtr DisplayFunc)

type EntryFunc = CInt -> IO ()

foreign import ccall "wrapper"
   makeEntryFunc :: EntryFunc -> IO (FunPtr EntryFunc)

type IdleFunc = IO ()

foreign import ccall "wrapper"
   makeIdleFunc :: IdleFunc -> IO (FunPtr IdleFunc)

type InitContextFunc = IO ()

foreign import ccall "wrapper"
   makeInitContextFunc :: InitContextFunc -> IO (FunPtr InitContextFunc)

type JoystickFunc = CUInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall "wrapper"
   makeJoystickFunc :: JoystickFunc -> IO (FunPtr JoystickFunc)

type KeyboardFunc = CUChar -> CInt -> CInt -> IO ()

foreign import ccall "wrapper"
   makeKeyboardFunc :: KeyboardFunc -> IO (FunPtr KeyboardFunc)

type KeyboardUpFunc = CUChar -> CInt -> CInt -> IO ()

foreign import ccall "wrapper"
   makeKeyboardUpFunc :: KeyboardUpFunc -> IO (FunPtr KeyboardUpFunc)

type MenuDestroyFunc = IO ()

foreign import ccall "wrapper"
   makeMenuDestroyFunc :: MenuDestroyFunc -> IO (FunPtr MenuDestroyFunc)

type MenuFunc = CInt -> IO ()

foreign import ccall "wrapper"
   makeMenuFunc :: MenuFunc -> IO (FunPtr MenuFunc)

type MenuStateFunc = CInt -> IO ()

foreign import ccall "wrapper"
   makeMenuStateFunc :: MenuStateFunc -> IO (FunPtr MenuStateFunc)

type MenuStatusFunc = CInt -> CInt -> CInt -> IO ()

foreign import ccall "wrapper"
   makeMenuStatusFunc :: MenuStatusFunc -> IO (FunPtr MenuStatusFunc)

type MotionFunc = CInt -> CInt -> IO ()

foreign import ccall "wrapper"
   makeMotionFunc :: MotionFunc -> IO (FunPtr MotionFunc)

type MouseFunc = CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall "wrapper"
   makeMouseFunc :: MouseFunc -> IO (FunPtr MouseFunc)

type MouseWheelFunc = CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall "wrapper"
   makeMouseWheelFunc :: MouseWheelFunc -> IO (FunPtr MouseWheelFunc)

type MultiButtonFunc = CInt -> CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall "wrapper"
   makeMultiButtonFunc :: MultiButtonFunc -> IO (FunPtr MultiButtonFunc)

type MultiEntryFunc = CInt -> CInt -> IO ()

foreign import ccall "wrapper"
   makeMultiEntryFunc :: MultiEntryFunc -> IO (FunPtr MultiEntryFunc)

type MultiMotionFunc = CInt -> CInt -> CInt -> IO ()

foreign import ccall "wrapper"
   makeMultiMotionFunc :: MultiMotionFunc -> IO (FunPtr MultiMotionFunc)

type MultiPassiveFunc = CInt -> CInt -> CInt -> IO ()

foreign import ccall "wrapper"
   makeMultiPassiveFunc :: MultiPassiveFunc -> IO (FunPtr MultiPassiveFunc)

type OverlayDisplayFunc = IO ()

foreign import ccall "wrapper"
   makeOverlayDisplayFunc :: OverlayDisplayFunc -> IO (FunPtr OverlayDisplayFunc)

type PassiveMotionFunc = CInt -> CInt -> IO ()

foreign import ccall "wrapper"
   makePassiveMotionFunc :: PassiveMotionFunc -> IO (FunPtr PassiveMotionFunc)

type PositionFunc = CInt -> CInt -> IO ()

foreign import ccall "wrapper"
   makePositionFunc :: PositionFunc -> IO (FunPtr PositionFunc)

type ReshapeFunc = CInt -> CInt -> IO ()

foreign import ccall "wrapper"
   makeReshapeFunc :: ReshapeFunc -> IO (FunPtr ReshapeFunc)

type SpaceballButtonFunc = CInt -> CInt -> IO ()

foreign import ccall "wrapper"
   makeSpaceballButtonFunc :: SpaceballButtonFunc -> IO (FunPtr SpaceballButtonFunc)

type SpaceballMotionFunc = CInt -> CInt -> CInt -> IO ()

foreign import ccall "wrapper"
   makeSpaceballMotionFunc :: SpaceballMotionFunc -> IO (FunPtr SpaceballMotionFunc)

type SpaceballRotateFunc = CInt -> CInt -> CInt -> IO ()

foreign import ccall "wrapper"
   makeSpaceballRotateFunc :: SpaceballRotateFunc -> IO (FunPtr SpaceballRotateFunc)

type SpecialFunc = CInt -> CInt -> CInt -> IO ()

foreign import ccall "wrapper"
   makeSpecialFunc :: SpecialFunc -> IO (FunPtr SpecialFunc)

type SpecialUpFunc = CInt -> CInt -> CInt -> IO ()

foreign import ccall "wrapper"
   makeSpecialUpFunc :: SpecialUpFunc -> IO (FunPtr SpecialUpFunc)

type TabletButtonFunc = CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall "wrapper"
   makeTabletButtonFunc :: TabletButtonFunc -> IO (FunPtr TabletButtonFunc)

type TabletMotionFunc = CInt -> CInt -> IO ()

foreign import ccall "wrapper"
   makeTabletMotionFunc :: TabletMotionFunc -> IO (FunPtr TabletMotionFunc)

type TimerFunc = CInt -> IO ()

foreign import ccall "wrapper"
   makeTimerFunc :: TimerFunc -> IO (FunPtr TimerFunc)

type VisibilityFunc = CInt -> IO ()

foreign import ccall "wrapper"
   makeVisibilityFunc :: VisibilityFunc -> IO (FunPtr VisibilityFunc)

type WMCloseFunc = IO ()

foreign import ccall "wrapper"
   makeWMCloseFunc :: WMCloseFunc -> IO (FunPtr WMCloseFunc)

type WindowStatusFunc = CInt -> IO ()

foreign import ccall "wrapper"
   makeWindowStatusFunc :: WindowStatusFunc -> IO (FunPtr WindowStatusFunc)
