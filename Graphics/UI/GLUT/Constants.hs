-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Constants
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This purely internal module defines all numeric GLUT constants.
--
-----------------------------------------------------------------------------

module Graphics.UI.GLUT.Constants where

import Foreign.C.Types ( CInt, CUInt )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )

-----------------------------------------------------------------------------
-- * Display mode bit masks
glut_RGB, glut_RGBA, glut_INDEX, glut_SINGLE, glut_DOUBLE, glut_ACCUM,
   glut_ALPHA, glut_DEPTH, glut_STENCIL, glut_MULTISAMPLE, glut_STEREO,
   glut_LUMINANCE :: CUInt
glut_RGB                               = 0x0000
glut_RGBA                              = glut_RGB
glut_INDEX                             = 0x0001
glut_SINGLE                            = 0x0000
glut_DOUBLE                            = 0x0002
glut_ACCUM                             = 0x0004
glut_ALPHA                             = 0x0008
glut_DEPTH                             = 0x0010
glut_STENCIL                           = 0x0020
glut_MULTISAMPLE                       = 0x0080
glut_STEREO                            = 0x0100
glut_LUMINANCE                         = 0x0200

-----------------------------------------------------------------------------
-- * Mouse buttons
glut_LEFT_BUTTON, glut_MIDDLE_BUTTON, glut_RIGHT_BUTTON, glut_WHEEL_UP,
   glut_WHEEL_DOWN :: CInt
glut_LEFT_BUTTON                       = 0
glut_MIDDLE_BUTTON                     = 1
glut_RIGHT_BUTTON                      = 2
glut_WHEEL_UP                          = 3
glut_WHEEL_DOWN                        = 4

-----------------------------------------------------------------------------
-- * Mouse button  state
glut_DOWN, glut_UP :: CInt
glut_DOWN                              = 0
glut_UP                                = 1

-----------------------------------------------------------------------------
-- * Function keys
glut_KEY_F1, glut_KEY_F2, glut_KEY_F3, glut_KEY_F4, glut_KEY_F5, glut_KEY_F6,
   glut_KEY_F7, glut_KEY_F8, glut_KEY_F9, glut_KEY_F10, glut_KEY_F11,
   glut_KEY_F12 :: CInt
glut_KEY_F1                            = 1
glut_KEY_F2                            = 2
glut_KEY_F3                            = 3
glut_KEY_F4                            = 4
glut_KEY_F5                            = 5
glut_KEY_F6                            = 6
glut_KEY_F7                            = 7
glut_KEY_F8                            = 8
glut_KEY_F9                            = 9
glut_KEY_F10                           = 10
glut_KEY_F11                           = 11
glut_KEY_F12                           = 12

-----------------------------------------------------------------------------
-- * Directional Keys
glut_KEY_LEFT, glut_KEY_UP, glut_KEY_RIGHT, glut_KEY_DOWN, glut_KEY_PAGE_UP,
   glut_KEY_PAGE_DOWN, glut_KEY_HOME, glut_KEY_END, glut_KEY_INSERT :: CInt
glut_KEY_LEFT                          = 100
glut_KEY_UP                            = 101
glut_KEY_RIGHT                         = 102
glut_KEY_DOWN                          = 103
glut_KEY_PAGE_UP                       = 104
glut_KEY_PAGE_DOWN                     = 105
glut_KEY_HOME                          = 106
glut_KEY_END                           = 107
glut_KEY_INSERT                        = 108

-----------------------------------------------------------------------------
-- * Entry\/exit state
glut_LEFT, glut_ENTERED :: CInt
glut_LEFT                              = 0
glut_ENTERED                           = 1

-----------------------------------------------------------------------------
-- * Menu usage state
glut_MENU_NOT_IN_USE, glut_MENU_IN_USE :: CInt
glut_MENU_NOT_IN_USE                   = 0
glut_MENU_IN_USE                       = 1

-----------------------------------------------------------------------------
-- * Visibility state
glut_NOT_VISIBLE, glut_VISIBLE :: CInt
glut_NOT_VISIBLE                       = 0
glut_VISIBLE                           = 1

-----------------------------------------------------------------------------
-- * Window status state
glut_HIDDEN, glut_FULLY_RETAINED, glut_PARTIALLY_RETAINED,
   glut_FULLY_COVERED :: CInt
glut_HIDDEN                            = 0
glut_FULLY_RETAINED                    = 1
glut_PARTIALLY_RETAINED                = 2
glut_FULLY_COVERED                     = 3

-----------------------------------------------------------------------------
-- * Color index component selection values
glut_RED, glut_GREEN, glut_BLUE :: CInt
glut_RED                               = 0
glut_GREEN                             = 1
glut_BLUE                              = 2

-----------------------------------------------------------------------------
-- * Layers in use
glut_NORMAL, glut_OVERLAY :: GLenum
glut_NORMAL                            = 0
glut_OVERLAY                           = 1

-----------------------------------------------------------------------------
-- * @glutGet@ parameters

glut_WINDOW_X, glut_WINDOW_Y, glut_WINDOW_WIDTH, glut_WINDOW_HEIGHT,
   glut_WINDOW_BUFFER_SIZE, glut_WINDOW_STENCIL_SIZE, glut_WINDOW_DEPTH_SIZE,
   glut_WINDOW_RED_SIZE, glut_WINDOW_GREEN_SIZE, glut_WINDOW_BLUE_SIZE,
   glut_WINDOW_ALPHA_SIZE, glut_WINDOW_ACCUM_RED_SIZE,
   glut_WINDOW_ACCUM_GREEN_SIZE, glut_WINDOW_ACCUM_BLUE_SIZE,
   glut_WINDOW_ACCUM_ALPHA_SIZE, glut_WINDOW_DOUBLEBUFFER, glut_WINDOW_RGBA,
   glut_WINDOW_PARENT, glut_WINDOW_NUM_CHILDREN, glut_WINDOW_COLORMAP_SIZE,
   glut_WINDOW_NUM_SAMPLES, glut_WINDOW_STEREO, glut_WINDOW_CURSOR,
   glut_SCREEN_WIDTH, glut_SCREEN_HEIGHT, glut_SCREEN_WIDTH_MM,
   glut_SCREEN_HEIGHT_MM, glut_MENU_NUM_ITEMS, glut_DISPLAY_MODE_POSSIBLE,
   glut_INIT_WINDOW_X, glut_INIT_WINDOW_Y, glut_INIT_WINDOW_WIDTH,
   glut_INIT_WINDOW_HEIGHT, glut_INIT_DISPLAY_MODE, glut_ELAPSED_TIME,
   glut_WINDOW_FORMAT_ID :: GLenum
glut_WINDOW_X                          = 100
glut_WINDOW_Y                          = 101
glut_WINDOW_WIDTH                      = 102
glut_WINDOW_HEIGHT                     = 103
glut_WINDOW_BUFFER_SIZE                = 104
glut_WINDOW_STENCIL_SIZE               = 105
glut_WINDOW_DEPTH_SIZE                 = 106
glut_WINDOW_RED_SIZE                   = 107
glut_WINDOW_GREEN_SIZE                 = 108
glut_WINDOW_BLUE_SIZE                  = 109
glut_WINDOW_ALPHA_SIZE                 = 110
glut_WINDOW_ACCUM_RED_SIZE             = 111
glut_WINDOW_ACCUM_GREEN_SIZE           = 112
glut_WINDOW_ACCUM_BLUE_SIZE            = 113
glut_WINDOW_ACCUM_ALPHA_SIZE           = 114
glut_WINDOW_DOUBLEBUFFER               = 115
glut_WINDOW_RGBA                       = 116
glut_WINDOW_PARENT                     = 117
glut_WINDOW_NUM_CHILDREN               = 118
glut_WINDOW_COLORMAP_SIZE              = 119
glut_WINDOW_NUM_SAMPLES                = 120
glut_WINDOW_STEREO                     = 121
glut_WINDOW_CURSOR                     = 122
glut_SCREEN_WIDTH                      = 200
glut_SCREEN_HEIGHT                     = 201
glut_SCREEN_WIDTH_MM                   = 202
glut_SCREEN_HEIGHT_MM                  = 203
glut_MENU_NUM_ITEMS                    = 300
glut_DISPLAY_MODE_POSSIBLE             = 400
glut_INIT_WINDOW_X                     = 500
glut_INIT_WINDOW_Y                     = 501
glut_INIT_WINDOW_WIDTH                 = 502
glut_INIT_WINDOW_HEIGHT                = 503
glut_INIT_DISPLAY_MODE                 = 504
glut_ELAPSED_TIME                      = 700
glut_WINDOW_FORMAT_ID                  = 123

-----------------------------------------------------------------------------
-- * @glutDeviceGet@ parameters
glut_HAS_KEYBOARD, glut_HAS_MOUSE, glut_HAS_SPACEBALL,
   glut_HAS_DIAL_AND_BUTTON_BOX, glut_HAS_TABLET, glut_NUM_MOUSE_BUTTONS,
   glut_NUM_SPACEBALL_BUTTONS, glut_NUM_BUTTON_BOX_BUTTONS, glut_NUM_DIALS,
   glut_NUM_TABLET_BUTTONS, glut_DEVICE_IGNORE_KEY_REPEAT,
   glut_DEVICE_KEY_REPEAT, glut_HAS_JOYSTICK, glut_OWNS_JOYSTICK,
   glut_JOYSTICK_BUTTONS, glut_JOYSTICK_AXES, glut_JOYSTICK_POLL_RATE :: GLenum
glut_HAS_KEYBOARD                      = 600
glut_HAS_MOUSE                         = 601
glut_HAS_SPACEBALL                     = 602
glut_HAS_DIAL_AND_BUTTON_BOX           = 603
glut_HAS_TABLET                        = 604
glut_NUM_MOUSE_BUTTONS                 = 605
glut_NUM_SPACEBALL_BUTTONS             = 606
glut_NUM_BUTTON_BOX_BUTTONS            = 607
glut_NUM_DIALS                         = 608
glut_NUM_TABLET_BUTTONS                = 609
glut_DEVICE_IGNORE_KEY_REPEAT          = 610
glut_DEVICE_KEY_REPEAT                 = 611
glut_HAS_JOYSTICK                      = 612
glut_OWNS_JOYSTICK                     = 613
glut_JOYSTICK_BUTTONS                  = 614
glut_JOYSTICK_AXES                     = 615
glut_JOYSTICK_POLL_RATE                = 616

-----------------------------------------------------------------------------
-- * @glutLayerGet@ parameters
glut_OVERLAY_POSSIBLE, glut_LAYER_IN_USE, glut_HAS_OVERLAY,
   glut_TRANSPARENT_INDEX, glut_NORMAL_DAMAGED, glut_OVERLAY_DAMAGED :: GLenum

glut_OVERLAY_POSSIBLE                  = 800
glut_LAYER_IN_USE                      = 801
glut_HAS_OVERLAY                       = 802
glut_TRANSPARENT_INDEX                 = 803
glut_NORMAL_DAMAGED                    = 804
glut_OVERLAY_DAMAGED                   = 805

-----------------------------------------------------------------------------
-- * @glutVideoResizeGet@ parameters
glut_VIDEO_RESIZE_POSSIBLE, glut_VIDEO_RESIZE_IN_USE,
   glut_VIDEO_RESIZE_X_DELTA, glut_VIDEO_RESIZE_Y_DELTA,
   glut_VIDEO_RESIZE_WIDTH_DELTA, glut_VIDEO_RESIZE_HEIGHT_DELTA,
   glut_VIDEO_RESIZE_X, glut_VIDEO_RESIZE_Y, glut_VIDEO_RESIZE_WIDTH,
   glut_VIDEO_RESIZE_HEIGHT :: CInt
glut_VIDEO_RESIZE_POSSIBLE             = 900
glut_VIDEO_RESIZE_IN_USE               = 901
glut_VIDEO_RESIZE_X_DELTA              = 902
glut_VIDEO_RESIZE_Y_DELTA              = 903
glut_VIDEO_RESIZE_WIDTH_DELTA          = 904
glut_VIDEO_RESIZE_HEIGHT_DELTA         = 905
glut_VIDEO_RESIZE_X                    = 906
glut_VIDEO_RESIZE_Y                    = 907
glut_VIDEO_RESIZE_WIDTH                = 908
glut_VIDEO_RESIZE_HEIGHT               = 909

-----------------------------------------------------------------------------
-- * @glutGetModifiers@ return mask
glut_ACTIVE_SHIFT, glut_ACTIVE_CTRL, glut_ACTIVE_ALT :: CInt
glut_ACTIVE_SHIFT                      = 0x01
glut_ACTIVE_CTRL                       = 0x02
glut_ACTIVE_ALT                        = 0x04

-----------------------------------------------------------------------------
-- * @glutSetCursor@ parameters
glut_CURSOR_RIGHT_ARROW, glut_CURSOR_LEFT_ARROW, glut_CURSOR_INFO,
   glut_CURSOR_DESTROY, glut_CURSOR_HELP, glut_CURSOR_CYCLE,
   glut_CURSOR_SPRAY, glut_CURSOR_WAIT, glut_CURSOR_TEXT,
   glut_CURSOR_CROSSHAIR, glut_CURSOR_UP_DOWN, glut_CURSOR_LEFT_RIGHT,
   glut_CURSOR_TOP_SIDE, glut_CURSOR_BOTTOM_SIDE, glut_CURSOR_LEFT_SIDE,
   glut_CURSOR_RIGHT_SIDE, glut_CURSOR_TOP_LEFT_CORNER,
   glut_CURSOR_TOP_RIGHT_CORNER, glut_CURSOR_BOTTOM_RIGHT_CORNER,
   glut_CURSOR_BOTTOM_LEFT_CORNER, glut_CURSOR_INHERIT, glut_CURSOR_NONE,
   glut_CURSOR_FULL_CROSSHAIR :: CInt
glut_CURSOR_RIGHT_ARROW                = 0
glut_CURSOR_LEFT_ARROW                 = 1
glut_CURSOR_INFO                       = 2
glut_CURSOR_DESTROY                    = 3
glut_CURSOR_HELP                       = 4
glut_CURSOR_CYCLE                      = 5
glut_CURSOR_SPRAY                      = 6
glut_CURSOR_WAIT                       = 7
glut_CURSOR_TEXT                       = 8
glut_CURSOR_CROSSHAIR                  = 9
glut_CURSOR_UP_DOWN                    = 10
glut_CURSOR_LEFT_RIGHT                 = 11
glut_CURSOR_TOP_SIDE                   = 12
glut_CURSOR_BOTTOM_SIDE                = 13
glut_CURSOR_LEFT_SIDE                  = 14
glut_CURSOR_RIGHT_SIDE                 = 15
glut_CURSOR_TOP_LEFT_CORNER            = 16
glut_CURSOR_TOP_RIGHT_CORNER           = 17
glut_CURSOR_BOTTOM_RIGHT_CORNER        = 18
glut_CURSOR_BOTTOM_LEFT_CORNER         = 19
glut_CURSOR_INHERIT                    = 100
glut_CURSOR_NONE                       = 101
glut_CURSOR_FULL_CROSSHAIR             = 102

-----------------------------------------------------------------------------
-- * @glutSetKeyRepeat@ modes
glut_KEY_REPEAT_OFF, glut_KEY_REPEAT_ON, glut_KEY_REPEAT_DEFAULT :: CInt
glut_KEY_REPEAT_OFF                    = 0
glut_KEY_REPEAT_ON                     = 1
glut_KEY_REPEAT_DEFAULT                = 2

-----------------------------------------------------------------------------
-- * Joystick button masks
glut_JOYSTICK_BUTTON_A, glut_JOYSTICK_BUTTON_B, glut_JOYSTICK_BUTTON_C,
   glut_JOYSTICK_BUTTON_D :: CUInt
glut_JOYSTICK_BUTTON_A                 = 0x01
glut_JOYSTICK_BUTTON_B                 = 0x02
glut_JOYSTICK_BUTTON_C                 = 0x04
glut_JOYSTICK_BUTTON_D                 = 0x08

-----------------------------------------------------------------------------
-- @glutGameModeGet@ parameters
glut_GAME_MODE_ACTIVE, glut_GAME_MODE_POSSIBLE, glut_GAME_MODE_WIDTH,
   glut_GAME_MODE_HEIGHT, glut_GAME_MODE_PIXEL_DEPTH,
   glut_GAME_MODE_REFRESH_RATE, glut_GAME_MODE_DISPLAY_CHANGED :: GLenum
glut_GAME_MODE_ACTIVE                  = 0
glut_GAME_MODE_POSSIBLE                = 1
glut_GAME_MODE_WIDTH                   = 2
glut_GAME_MODE_HEIGHT                  = 3
glut_GAME_MODE_PIXEL_DEPTH             = 4
glut_GAME_MODE_REFRESH_RATE            = 5
glut_GAME_MODE_DISPLAY_CHANGED         = 6
