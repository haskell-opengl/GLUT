--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Menu
-- Copyright   :  (c) Sven Panne 2002
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  experimental
-- Portability :  portable
--
-- GLUT supports simple cascading pop-up menus. They are designed to let a user
-- select various modes within a program. The functionality is simple and
-- minimalistic and is meant to be that way. Do not mistake GLUT\'s pop-up menu
-- facility with an attempt to create a full-featured user interface.
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.Menu (
   Menu(..), MenuItem(..), MenuCallback, attachMenu
) where

import Data.Array ( listArray, (!) )
import Data.FiniteMap ( FiniteMap, emptyFM, lookupFM, addToFM, delFromFM )
import Data.IORef ( IORef, newIORef, readIORef, modifyIORef )
import Data.Maybe ( fromJust )
import Foreign.C.String ( CString, withCString )
import Foreign.C.Types ( CInt )
import Foreign.Ptr ( FunPtr, freeHaskellFunPtr )
import Control.Monad ( liftM, unless, zipWithM )
import System.IO.Unsafe ( unsafePerformIO )
import Graphics.UI.GLUT.Callbacks.Window ( MouseButton, marshalMouseButton )
import Graphics.UI.GLUT.Window ( Window, getWindow )

--------------------------------------------------------------------------------

-- | A menu is simply a list of menu items.
newtype Menu = Menu [MenuItem]

-- | A single item within a menu can either be a plain menu entry or a sub-menu
-- entry, allowing for arbitrarily deep nested menus.
data MenuItem
   = MenuEntry String MenuCallback -- ^ A plain menu entry with an associated
                                   --   callback, which is triggered when the
                                   --   user selects the entry
   | SubMenu   String Menu         -- ^ A sub-menu, which is cascaded when the
                                   --   user selects the entry, allowing
                                   --   sub-menu entries to be selected

type MenuCallback = IO ()

-- | Create a new pop-up menu for the /current window,/ attaching it to the
-- given mouse button. A previously attached menu (if any), is detached before
-- and won\'t receive callbacks anymore.
--
-- It is illegal to call 'attachMenu' while any (sub-)menu is in use, i.e.
-- popped up.
--
-- /X Implementation Notes:/ If available, GLUT for X will take advantage of
-- overlay planes for implementing pop-up menus. The use of overlay planes can
-- eliminate display callbacks when pop-up menus are deactivated. The
-- @SERVER_OVERLAY_VISUALS@ convention is used to determine if overlay visuals
-- are available.

attachMenu :: MouseButton -> Menu -> IO ()
attachMenu mouseButton menu@(Menu items) = do
   maybeWindow <- getWindow
   let hook = MenuHook (fromJust maybeWindow) mouseButton
   detachMenu hook
   unless (null items) $ do
      (menuID, destructor) <- traverseMenu menu
      addToMenuTable hook destructor
      attachMenu_ mouseButton

detachMenu :: MenuHook -> IO ()
detachMenu hook@(MenuHook _ mouseButton) = do
   maybeDestructor <- lookupInMenuTable hook
   case maybeDestructor of
      Nothing         -> return ()
      Just destructor -> do detachMenu_ mouseButton
                            destructor
   deleteFromMenuTable hook

traverseMenu :: Menu -> IO (MenuID, Destructor)
traverseMenu (Menu items) = do
   let callbackArray = listArray (1, length items) (map makeCallback items)
   cb <- makeMenuFunc (\i -> callbackArray ! (fromIntegral i))
   menuID <- glutCreateMenu cb
   destructors <- zipWithM addMenuItem items [1..]
   let destructor = do sequence_ destructors
                       glutDestroyMenu menuID
                       freeHaskellFunPtr cb
   return (menuID, destructor)

makeCallback :: MenuItem -> MenuCallback
makeCallback (MenuEntry _ cb) = cb
makeCallback _ = error "shouldn't receive a callback for submenus"

addMenuItem :: MenuItem -> Value -> IO Destructor
addMenuItem (MenuEntry s _) v = do
   addMenuEntry s v
   return $ glutRemoveMenuItem 1
addMenuItem (SubMenu s m) _ = do
   (menuID, destructor) <- saveExcursion (traverseMenu m)
   addSubMenu s menuID
   return $ do glutRemoveMenuItem 1
               destructor

-- Perform an action, saving/restoring the current menu around it
saveExcursion :: IO a -> IO a
saveExcursion act = do
   maybeMenuID <- getMenu
   returnValue <- act
   maybe (return ()) setMenu maybeMenuID      -- Hmmm, a bit to lenient?
   return returnValue

--------------------------------------------------------------------------------
-- This seems to be a common Haskell hack nowadays: A plain old global variable
-- with an associated mutator. Perhaps some language/library support is needed?

{-# notInline theMenuTable #-}
theMenuTable :: IORef MenuTable
theMenuTable = unsafePerformIO (newIORef emptyMenuTable)

getMenuTable :: IO MenuTable
getMenuTable = readIORef theMenuTable

modifyMenuTable :: (MenuTable -> MenuTable) -> IO ()
modifyMenuTable = modifyIORef theMenuTable

--------------------------------------------------------------------------------
-- To facilitate cleanup, we have to keep track how to destroy menus which are
-- currently attached in a window to a mouse button.

data MenuHook = MenuHook Window MouseButton deriving ( Eq, Ord )

type Destructor = IO ()

type MenuTable = FiniteMap MenuHook Destructor

emptyMenuTable :: MenuTable
emptyMenuTable = emptyFM

lookupInMenuTable :: MenuHook -> IO (Maybe Destructor)
lookupInMenuTable callbackID =
   liftM (flip lookupFM callbackID) getMenuTable

deleteFromMenuTable :: MenuHook -> IO ()
deleteFromMenuTable callbackID =
   modifyMenuTable (flip delFromFM callbackID)

addToMenuTable :: MenuHook -> Destructor -> IO ()
addToMenuTable callbackID funPtr =
   modifyMenuTable (\table -> addToFM table callbackID funPtr)

--------------------------------------------------------------------------------

type MenuID = CInt
type Value  = CInt
type Item   = CInt

--------------------------------------------------------------------------------

-- | The type of a menu callback action that is called when a menu entry from a
-- menu is selected. The value passed to the callback is determined by the value
-- for the selected menu entry.

type MenuCB = CInt -> IO ()

-- | Create a new pop-up menu and return a unique identifier for it, which can
-- be used when calling 'setMenu'. Implicitly, the /current menu/ is set to the
-- newly created menu.
--
-- When the menu callback is called because a menu entry is selected for the
-- menu, the /current menu/ will be implicitly set to the menu with the selected
-- entry before the callback is made.
--
-- /X Implementation Notes:/ If available, GLUT for X will take advantage of
-- overlay planes for implementing pop-up menus. The use of overlay planes can
-- eliminate display callbacks when pop-up menus are deactivated. The
-- @SERVER_OVERLAY_VISUALS@ convention is used to determine if overlay visuals
-- are available.

foreign import ccall unsafe "glutCreateMenu" glutCreateMenu ::
   FunPtr MenuCB -> IO MenuID

foreign import ccall "wrapper" makeMenuFunc :: MenuCB -> IO (FunPtr MenuCB)

-- | Destroy the specified menu. If it was the /current menu/, the /current
-- menu/ becomes invalid and 'getMenu' will return 'Nothing'.

foreign import ccall unsafe "glutDestroyMenu" glutDestroyMenu ::
   MenuID -> IO ()

--------------------------------------------------------------------------------

-- | Set the /current menu./

foreign import ccall unsafe "glutSetMenu" setMenu :: MenuID -> IO ()

-- | Return 'Just' the identifier of the /current menu./ 'Nothing' is returned
-- if no menus exist or the previous /current menu/ was destroyed.

getMenu :: IO (Maybe MenuID)
getMenu = liftM (\i -> if i == 0 then Nothing else Just i) glutGetMenu

foreign import ccall unsafe "glutGetMenu" glutGetMenu :: IO MenuID

--------------------------------------------------------------------------------

-- | Add a menu entry to the bottom of the /current menu./ The given string will
-- be displayed for the newly added menu entry. If the menu entry is selected by
-- the user, the menu\'s callback will be called passing the given value as the
-- callback\'s parameter.

addMenuEntry :: String -> Value -> IO ()
addMenuEntry name value = withCString name $ \n -> glutAddMenuEntry n value

foreign import ccall unsafe "glutAddMenuEntry" glutAddMenuEntry ::
   CString -> Value -> IO ()

-- | Add a sub-menu trigger to the bottom of the /current menu./ The given
-- string will be displayed for the newly added sub-menu trigger. If the
-- sub-menu trigger is entered, the sub-menu specified by the given menu
-- identifier will be cascaded, allowing sub-menu menu items to be selected.

addSubMenu :: String -> MenuID -> IO ()
addSubMenu name menuID = withCString name $ \n -> glutAddSubMenu n menuID

foreign import ccall unsafe "glutAddSubMenu" glutAddSubMenu ::
   CString -> MenuID -> IO ()

--------------------------------------------------------------------------------

{- UNUSED
-- | Change the specified menu entry in the /current menu/ into a menu entry.
-- The given position determines which menu item should be changed and must be
-- between 1 (the topmost menu item) and
-- 'Graphics.UI.GLUT.State.getNumMenuItems' inclusive. The menu item to change
-- does not have to be a menu entry already. The given string will be displayed
-- for the newly changed menu entry. The given value will be returned to the
-- menu\'s callback if this menu entry is selected.

foreign import ccall unsafe "glutChangeToMenuEntry" glutChangeToMenuEntry ::
   Item -> CString -> Value -> IO ()

-- | Change the specified menu item in the /current menu/ into a sub-menu
-- trigger. The  given position determines which menu item should be changed and
-- must be between 1 and 'Graphics.UI.GLUT.State.getNumMenuItems' inclusive. The
-- menu item to change does not have to be a sub-menu trigger already. The
-- given name will be displayed for the newly changed sub-menu trigger. The
-- given menu identifier names the sub-menu to cascade from the newly added
-- sub-menu trigger.

foreign import ccall unsafe "glutChangeToSubMenu" glutChangeToSubMenu ::
   Item -> CString -> MenuID -> IO ()
-}

--------------------------------------------------------------------------------

-- | Remove the menu item at the given position, regardless of whether it is a
-- menu entry or sub-menu trigger. The position must be between 1 (the topmost
-- menu item) and 'Graphics.UI.GLUT.State.getNumMenuItems' inclusive. Menu items
-- below the removed menu item are renumbered.

foreign import ccall unsafe "glutRemoveMenuItem" glutRemoveMenuItem ::
   Item -> IO ()

--------------------------------------------------------------------------------

-- | Attach a mouse button for the /current window/ to the identifier of the
-- /current menu./ By attaching a menu identifier to a button, the named menu
-- will be popped up when the user presses the specified button. Note that the
-- menu is attached to the button by identifier, not by reference.


attachMenu_ :: MouseButton -> IO ()
attachMenu_ = glutAttachMenu . marshalMouseButton

foreign import ccall unsafe "glutAttachMenu" glutAttachMenu :: CInt -> IO ()

-- | Detach an attached mouse button from the /current window./

detachMenu_ :: MouseButton -> IO ()
detachMenu_ = glutDetachMenu . marshalMouseButton

foreign import ccall unsafe "glutDetachMenu" glutDetachMenu :: CInt -> IO ()
