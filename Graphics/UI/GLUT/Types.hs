-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Types
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module with miscellaneous types which don\'t really
-- have a good place elsewhere.
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.Types (
   Window,                                   -- constructor used only internally
   makeWindow,                               -- used only internally
   Relation(..),
   relationToString,                         -- used only internally
   MouseButton(..),
   marshalMouseButton, unmarshalMouseButton  -- used only internally
) where

import Foreign.C.Types ( CInt )
import Graphics.UI.GLUT.Constants (
   glut_LEFT_BUTTON, glut_MIDDLE_BUTTON, glut_RIGHT_BUTTON,
   glut_WHEEL_UP, glut_WHEEL_DOWN )

--------------------------------------------------------------------------------

-- | An opaque identifier for a top-level window or a subwindow.

newtype Window = Window CInt
   deriving ( Eq, Ord, Show )

makeWindow :: CInt -> Window
makeWindow = Window

--------------------------------------------------------------------------------

-- | A relation between a 'Graphics.UI.GLUT.Initialization.DisplayCapability'
-- and a numeric value.

data Relation
   = IsEqualTo        -- ^ Equal.
   | IsNotEqualTo     -- ^ Not equal.
   | IsLessThan       -- ^ Less than and preferring larger difference (the least
                      --   is best).
   | IsNotGreaterThan -- ^ Less than or equal and preferring larger difference
                      --   (the least is best).
   | IsGreaterThan    -- ^ Greater than and preferring larger differences (the
                      --   most is best).
   | IsAtLeast        -- ^ Greater than or equal and preferring more instead of
                      --   less. This relation is useful for allocating
                      --   resources like color precision or depth buffer
                      --   precision where the maximum precision is generally
                      --   preferred. Contrast with 'IsNotLessThan' relation.
   | IsNotLessThan    -- ^ Greater than or equal but preferring less instead of
                      --   more. This relation is useful for allocating
                      --   resources such as stencil bits or auxillary color
                      --   buffers where you would rather not over-allocate.
   deriving ( Eq, Ord, Show )

relationToString :: Relation -> String
relationToString IsEqualTo        = "="
relationToString IsNotEqualTo     = "!="
relationToString IsLessThan       = "<"
relationToString IsNotGreaterThan = "<="
relationToString IsGreaterThan    = ">"
relationToString IsAtLeast        = ">="
relationToString IsNotLessThan    = "~"

--------------------------------------------------------------------------------

-- | Mouse buttons, including a wheel

data MouseButton
   = LeftButton
   | MiddleButton
   | RightButton
   | WheelUp
   | WheelDown
   deriving ( Eq, Ord, Show )

marshalMouseButton :: MouseButton -> CInt
marshalMouseButton x = case x of
   LeftButton -> glut_LEFT_BUTTON
   MiddleButton -> glut_MIDDLE_BUTTON
   RightButton -> glut_RIGHT_BUTTON
   WheelUp ->glut_WHEEL_UP
   WheelDown -> glut_WHEEL_DOWN

unmarshalMouseButton :: CInt -> MouseButton
unmarshalMouseButton x
   | x == glut_LEFT_BUTTON = LeftButton
   | x == glut_MIDDLE_BUTTON = MiddleButton
   | x == glut_RIGHT_BUTTON = RightButton
   | x == glut_WHEEL_UP = WheelUp
   | x == glut_WHEEL_DOWN = WheelDown
   | otherwise = error ("unmarshalMouseButton: illegal value " ++ show x)
