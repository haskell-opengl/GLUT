-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- A Haskell binding for GLUT, the OpenGL Utility Toolkit, a window system
-- independent toolkit for writing OpenGL programs.
--
-----------------------------------------------------------------------------

module Graphics.UI.GLUT (
   -- * Legal stuff

   -- $LegalStuff

   -- * Introduction

   -- $Introduction

   -- ** Background

   -- $Background

   -- ** Design Philosophy

   -- $DesignPhilosophy

   -- ** API Versions

   -- $APIVersions

   -- ** Conventions

   -- $Conventions

   -- ** Terminology

   -- $Terminology

   module Graphics.Rendering.OpenGL,

   module Graphics.UI.GLUT.Initialization,
   module Graphics.UI.GLUT.Begin,
   module Graphics.UI.GLUT.Window,
   module Graphics.UI.GLUT.Overlay,
   module Graphics.UI.GLUT.Menu,
   module Graphics.UI.GLUT.Callbacks,
   module Graphics.UI.GLUT.Colormap,
   module Graphics.UI.GLUT.State,
   module Graphics.UI.GLUT.Fonts,
   module Graphics.UI.GLUT.Objects,
   module Graphics.UI.GLUT.Debugging,
   module Graphics.UI.GLUT.DeviceControl,
   module Graphics.UI.GLUT.GameMode
)  where

import Graphics.Rendering.OpenGL

import Graphics.UI.GLUT.Initialization
import Graphics.UI.GLUT.Begin
import Graphics.UI.GLUT.Window
import Graphics.UI.GLUT.Overlay
import Graphics.UI.GLUT.Menu
import Graphics.UI.GLUT.Callbacks
import Graphics.UI.GLUT.Colormap
import Graphics.UI.GLUT.State
import Graphics.UI.GLUT.Fonts
import Graphics.UI.GLUT.Objects
import Graphics.UI.GLUT.Debugging
import Graphics.UI.GLUT.DeviceControl
import Graphics.UI.GLUT.GameMode

-----------------------------------------------------------------------------
-- $LegalStuff
-- This documentation is heavily based on the man pages of Mark J. Kilgard\'s
-- GLUT library.
--
-- OpenGL is a trademark of Silicon Graphics, Inc.
-- X Window System is a trademark of X Consortium, Inc.
-- Spaceball is a registered trademark of Spatial Systems, Inc.
--
-- The author has taken care in preparation of this documentation but makes
-- no expressed or implied warranty of any kind and assumes no responsibility
-- for errors or omissions. No liability is assumed for incidental or
-- consequential damages in connection with or arising from the use of
-- information or programs contained herein.

-----------------------------------------------------------------------------
-- $Introduction
-- The OpenGL Utility Toolkit (GLUT) is a programming interface for writing
-- window system independent OpenGL programs. Currently there are
-- implementations for the X Window System, the Windows family, OS\/2, and Mac.
-- The toolkit supports the following functionality:
--
-- * Multiple windows for OpenGL rendering.
--
-- * Callback driven event processing.
--
-- * Sophisticated input devices.
--
-- * An /idle/ routine and timers.
--
-- * A simple, cascading pop-up menu facility.
--
-- * Utility routines to generate various solid and wire frame objects.
--
-- * Support for bitmap and stroke fonts.
--
-- * Miscellaneous window management functions, including managing overlays.
--
-- This documentation serves as both a specification and a programming guide.
-- If you are interested in a brief introduction to programming with GLUT,
-- have a look at the relevant parts of <http://www.opengl.org/> and the vast
-- amount of books on OpenGL, most of them use GLUT.
--
-- The remainder of this section describes GLUT\'s design philosophy and
-- usage model. The following sections specify the GLUT routines, grouped by
-- functionality. The final sections discuss usage advice and the logical
-- programmer visible state maintained by GLUT.

-----------------------------------------------------------------------------
-- $Background
-- One of the major accomplishments in the specification of OpenGL was
-- the isolation of window system dependencies from OpenGL\'s rendering
-- model. The result is that OpenGL is window system independent.
--
-- Window system operations such as the creation of a rendering window and the
-- handling of window system events are left to the native window system to
-- define. Necessary interactions between OpenGL and the window system such as
-- creating and binding an OpenGL context to a window are described separately
-- from the OpenGL specification in a window system dependent specification. For
-- example, the GLX specification describes the standard by which OpenGL
-- interacts with the X Window System.
--
-- The predecessor to OpenGL is IRIS GL. Unlike OpenGL, IRIS GL /does/
-- specify how rendering windows are created and manipulated. IRIS GL\'s
-- windowing interface is reasonably popular largely because it is simple to
-- use. IRIS GL programmers can worry about graphics programming without needing
-- to be an expert in programming the native window system. Experience also
-- demonstrated that IRIS GL\'s windowing interface was high-level enough that
-- it could be retargeted to different window systems. Silicon Graphics migrated
-- from NeWS to the X Window System without any major changes to IRIS GL\'s
-- basic windowing interface.
--
-- Removing window system operations from OpenGL is a sound decision because it
-- allows the OpenGL graphics system to be retargeted to various systems
-- including powerful but expensive graphics workstations as well as
-- mass-production graphics systems like video games, set-top boxes for
-- interactive television, and PCs.
--
-- Unfortunately, the lack of a window system interface for OpenGL is a gap in
-- OpenGL\'s utility. Learning native window system APIs such as the X Window
-- System\'s Xlib or Motif can be daunting. Even those familiar with
-- native window system APIs need to understand the interface that binds OpenGL
-- to the native window system. And when an OpenGL program is written using the
-- native window system interface, despite the portability of the program\'s
-- OpenGL rendering code, the program itself will be window system dependent.
--
-- Testing and documenting OpenGL\'s functionality lead to the development of
-- the @tk@ and @aux@ toolkits. The @aux@ toolkit is used in the examples found
-- in the /OpenGL Programming Guide/. Unfortunately, @aux@ has numerous
-- limitations and its utility is largely limited to toy programs. The @tk@
-- library has more functionality than @aux@ but was developed in an /ad hoc/
-- fashion and still lacks much important functionality that IRIS GL programmers
-- expect, like pop-up menus and overlays.
--
-- GLUT is designed to fill the need for a window system independent programming
-- interface for OpenGL programs. The interface is designed to be simple yet
-- still meet the needs of useful OpenGL programs. Features from the IRIS GL,
-- @aux@, and @tk@ interfaces are included to make it easy for programmers used
-- to these interfaces to develop programs for GLUT.

-----------------------------------------------------------------------------
-- $DesignPhilosophy
-- GLUT simplifies the implementation of programs using OpenGL rendering. The
-- GLUT application programming interface (API) requires very few routines to
-- display a graphics scene rendered using OpenGL. The GLUT API (like the OpenGL
-- API) is stateful. Most initial GLUT state is defined and the initial state is
-- reasonable for simple programs. The GLUT routines also take relatively few
-- parameters.
--
-- The GLUT API is (as much as reasonable) window system independent. For this
-- reason, GLUT does not return /any/ native window system handles, pointers, or
-- other data structures. More subtle window system dependencies such as
-- reliance on window system dependent fonts are avoided by GLUT; instead, GLUT
-- supplies its own (limited) set of fonts.
--
-- For programming ease, GLUT provides a simple menu sub-API. While the menuing
-- support is designed to be implemented as pop-up menus, GLUT gives window
-- system leeway to support the menu functionality in another manner (pull-down
-- menus for example).
--
-- Two of the most important pieces of GLUT state are the /current window/ and
-- /current menu/. Most window and menu routines affect the /current window/ or
-- /menu/ respectively. Most callbacks implicitly set the /current window/ and
-- /menu/ to the appropriate window or menu responsible for the callback. GLUT
-- is designed so that a program with only a single window and\/or menu will not
-- need to keep track of any window or menu identifiers. This greatly simplifies
-- very simple GLUT programs.
--
-- GLUT is designed for simple to moderately complex programs focused on OpenGL
-- rendering. GLUT implements its own event loop. For this reason, mixing GLUT
-- with other APIs that demand their own event handling structure may be
-- difficult. The advantage of a builtin event dispatch loop is simplicity.
--
-- GLUT contains routines for rendering fonts and geometric objects, however
-- GLUT makes no claims on the OpenGL display list name space. For this reason,
-- none of the GLUT rendering routines use OpenGL display lists. It is up to the
-- GLUT programmer to compile the output from GLUT rendering routines into
-- display lists if this is desired.
--
-- GLUT routines are logically organized into several sub-APIs according to
-- their functionality. The sub-APIs are:
--
-- * /Initialization:/ Command line processing, window system initialization,
--   and initial window creation state are controlled by these routines.
--
-- * /Beginning Event Processing:/ This routine enters GLUT\'s event processing
--   loop. This routine never returns, and it continuously calls GLUT callbacks
--   as necessary.
--
-- * /Window Management:/ These routines create and control windows.
--
-- * /Overlay Management:/ These routines establish and manage overlays for
--   windows.
--
-- * /Menu Management:/ These routines create and control pop-up menus.
--
-- * /Callback Registration:/ These routines register callbacks to be called by
--   the GLUT event processing loop.
--
-- * /Color Index Colormap Management:/ These routines allow the manipulation
--   of color index colormaps for windows.
--
-- * /State Retrieval:/ These routines allows programs to retrieve state from
--   GLUT.
--
-- * /Font Rendering:/ These routines allow rendering of stroke and bitmap
--   fonts.
--
-- * /Geometric Shape Rendering:/ These routines allow the rendering of 3D
--   geometric objects including spheres, cones, icosahedrons, and teapots.
--
-- * /Debugging:/ This routine reports any pending GL errors.
--
-- * /Device Control:/ These routines allow setting the key repeat and polling
--   the joystick.
--
-- * /Game Mode:/ These routines allow programs to enter\/leave a full-screen
--   mode with specified properties.

-- Note that the following item has been left out intentionally, its
-- implementation is too SGI-specific:
-- * /Video Resizing:/ These routines provide a means for doing swap or frame
--   synchronous resizing\/panning of the area that is to be magnified (or
--   passed through) to the output video resolution.

-----------------------------------------------------------------------------
-- $APIVersions
-- The GLUT API has undergone several revisions with increasing functionality.
-- This Haskell binding provides access to everything in API version 4,
-- although it is not yet officially finalized. Nevertheless, it provides very
-- useful things like handling full-screen modes and special keys.

-----------------------------------------------------------------------------
-- $Conventions
-- GLUT window and screen coordinates are expressed in pixels. The upper
-- left hand corner of the screen or a window is (0,0). X coordinates
-- increase in a rightward direction; Y coordinates increase in a
-- downward direction. Note: This is inconsistent with OpenGL\'s
-- coordinate scheme that generally considers the lower left hand
-- coordinate of a window to be at (0,0) but is consistent with most
-- popular window systems.

-----------------------------------------------------------------------------
-- $Terminology
-- A number of terms are used in a GLUT-specific manner throughout this
-- document. The GLUT meaning of these terms is independent of the window
-- system GLUT is used with. Here are GLUT-specific meanings for the
-- following GLUT-specific terms:
--
-- * /Callback:/ A programmer specified routine that can be registered with
--   GLUT to be called in response to a specific type of event. Also used to
-- refer to a specific callback routine being called.
--
-- * /Colormap:/ A mapping of pixel values to RGB color values. Used by color
--   index windows.
--
-- * /Dials and button box:/ A sophisticated input device consisting of a pad
--   of buttons and an array of rotating dials, often used by computer-aided
--   design programs.
--
-- * /Display mode:/ A set of OpenGL frame buffer capabilities that can be
--   attributed to a window.
--
-- * /Idle:/ A state when no window system events are received for processing
--   as callbacks and the idle callback, if one is registered, is called.
--
-- * /Layer in use:/ Either the normal plane or overlay. This per-window state
--   determines what frame buffer layer OpenGL commands affect.
--
-- * /Menu entry:/ A menu item that the user can select to trigger the menu
--   callback for the menu entry\'s value.
--
-- * /Menu item:/ Either a menu entry or a sub-menu trigger.
--
-- * /Modifiers:/ The Shift, Ctrl, and Alt keys that can be held down
--   simultaneously with a key or mouse button being pressed or released.
--
-- * /Multisampling:/ A technique for hardware antialiasing generally available
--   only on expensive 3D graphics hardware. Each pixel is composed of a number
--   of samples (each containing color and depth information). The samples are
--   averaged to determine the displayed pixel color value. Multisampling is
--   supported as an extension to OpenGL.
--
-- * /Normal plane:/ The default frame buffer layer where GLUT window state
--   resides; as opposed to the /overlay/.
--
-- * /Overlay:/ A frame buffer layer that can be displayed preferentially to
--   the /normal plane/ and supports transparency to display through to the
--   /normal plane/. Overlays are useful for rubber-banding effects, text
--   annotation, and other operations, to avoid damaging the normal plane frame
--   buffer state. Overlays require hardware support not present on all systems.
--
-- * /Pop:/ The act of forcing a window to the top of the stacking order for
--   sibling windows.
--
-- * /Pop-up menu:/ A menu that can be set to appear when a specified mouse
--   button is pressed in a window. A pop-menu consists of multiple menu items.
--
-- * /Push:/ The act of forcing a window to the bottom of the stacking order
--   for sibling windows.
--
-- * /Reshape:/ The act of changing the size or shape of the window.
--
-- * /Spaceball:/ A sophisticated 3D input device that provides six degrees of
--   freedom, three axes of rotation and three axes of translation. It also
--   supports a number of buttons. The device is a hand-sized ball attached to
--   a base. By cupping the ball with one\'s hand and applying torsional or
--   directional force on the ball, rotations and translationsare generated.
--
-- * /Stereo:/ A frame buffer capability providing left and right color buffers
--   for creating stereoscopic renderings. Typically, the user wears LCD
--   shuttered goggles synchronized with the alternating display on the screen
--   of the left and right color buffers.
--
-- * /Sub-menu:/ A menu cascaded from some sub-menu trigger.
--
-- * /Sub-menu trigger:/ A menu item that the user can enter to cascade another
--   pop-up menu.
--
-- * /Subwindow:/ A type of window that is the child window of a top-level
--   window or other subwindow. The drawing and visible region of a subwindow
--   is limited by its parent window.
--
-- * /Tablet:/ A precise 2D input device. Like a mouse, 2D coordinates are
--   returned. The absolute position of the tablet \"puck\" on the tablet is
--   returned. Tablets also support a number of buttons.
--
-- * /Timer:/ A callback that can be scheduled to be called in a specified
--   interval of time.
--
-- * /Top-level window:/ A window that can be placed, moved, resized, etc.
--   independently from other top-level windows by the user. Subwindows may
--   reside within a top-level window.
--
-- * /Window:/ A rectangular area for OpenGL rendering.
--
-- * /Window display state:/ One of shown, hidden, or iconified. A shown window
--   is potentially visible on the screen (it may be obscured by other windows
--   and not actually visible). A hidden window will never be visible. An
--   iconified window is not visible but could be made visible in response to
--   some user action like clicking on the window\'s corresponding icon.
--
-- * /Window system:/ A broad notion that refers to both the mechanism and
--   policy of the window system. For example, in the X Window System both the
--   window manager and the X server are integral to what GLUT considers the
--   window system.
