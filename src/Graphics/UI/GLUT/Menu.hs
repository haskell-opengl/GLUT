{-# OPTIONS_GHC -fno-cse #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Menu
-- Copyright   :  (c) Sven Panne 2002-2018
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- GLUT supports simple cascading pop-up menus. They are designed to let a user
-- select various modes within a program. The functionality is simple and
-- minimalistic and is meant to be that way. Do not mistake GLUT\'s pop-up menu
-- facility with an attempt to create a full-featured user interface.
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.Menu (
   Menu(..), MenuItem(..), MenuCallback, attachMenu,
   numMenuItems
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad ( when, unless, zipWithM )
import Data.Array ( listArray, (!) )
import Data.IORef ( IORef, newIORef, readIORef, modifyIORef )
import qualified Data.Map as M
import Data.StateVar ( get, ($=), GettableStateVar, makeGettableStateVar
                     , StateVar, makeStateVar )
import Foreign.C.String ( withCString )
import Foreign.C.Types ( CInt )
import Foreign.Ptr ( freeHaskellFunPtr )
import System.IO.Unsafe ( unsafePerformIO )

import Graphics.UI.GLUT.Callbacks.Registration
import Graphics.UI.GLUT.QueryUtils
import Graphics.UI.GLUT.Raw
import Graphics.UI.GLUT.Types

--------------------------------------------------------------------------------

-- | A menu is simply a list of menu items, possibly with an associated font.
data Menu
   = Menu [MenuItem]
   | MenuWithFont BitmapFont [MenuItem]

menuFont :: Menu -> Maybe BitmapFont
menuFont (Menu _) = Nothing
menuFont (MenuWithFont font _) = Just font

menuItems :: Menu -> [MenuItem]
menuItems (Menu items) = items
menuItems (MenuWithFont _ items) = items

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

attachMenu :: MonadIO m => MouseButton -> Menu -> m ()
attachMenu mouseButton menu = liftIO $ do
   win <- getCurrentWindow "attachMenu"
   let hook = MenuHook win mouseButton
   detachMenu hook
   unless (null (menuItems menu)) $ do
      (_, destructor) <- traverseMenu menu
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
traverseMenu menu = do
   let items = menuItems menu
       callbackArray = listArray (1, length items) (map makeCallback items)
   cb <- makeMenuFunc (\i -> callbackArray ! (fromIntegral i))
   menuID <- glutCreateMenu cb
   maybe (return ()) (setMenuFont menuID) (menuFont menu)
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
   menuID <- get currentMenu
   returnValue <- act
   when (isRealMenu menuID) $
      currentMenu $= menuID
   return returnValue

--------------------------------------------------------------------------------
-- This seems to be a common Haskell hack nowadays: A plain old global variable
-- with an associated mutator. Perhaps some language/library support is needed?

{-# NOINLINE theMenuTable #-}
theMenuTable :: IORef MenuTable
theMenuTable = unsafePerformIO (newIORef emptyMenuTable)

getMenuTable :: IO MenuTable
getMenuTable = readIORef theMenuTable

modifyMenuTable :: (MenuTable -> MenuTable) -> IO ()
modifyMenuTable = modifyIORef theMenuTable

--------------------------------------------------------------------------------
-- To facilitate cleanup, we have to keep track how to destroy menus which are
-- currently attached in a window to a mouse button.

data MenuHook = MenuHook Window MouseButton
   deriving ( Eq, Ord )

type Destructor = IO ()

type MenuTable = M.Map MenuHook Destructor

emptyMenuTable :: MenuTable
emptyMenuTable = M.empty

lookupInMenuTable :: MenuHook -> IO (Maybe Destructor)
lookupInMenuTable callbackID =
   fmap (M.lookup callbackID) getMenuTable

deleteFromMenuTable :: MenuHook -> IO ()
deleteFromMenuTable callbackID =
   modifyMenuTable (M.delete callbackID)

addToMenuTable :: MenuHook -> Destructor -> IO ()
addToMenuTable callbackID funPtr =
   modifyMenuTable (M.insert callbackID funPtr)

--------------------------------------------------------------------------------

type MenuID = CInt
type Value  = CInt

--------------------------------------------------------------------------------

-- | Controls the /current menu./ If no menus exist or the previous /current
-- menu/ was destroyed, a pseudo menu is returned.

currentMenu :: StateVar MenuID
currentMenu = makeStateVar glutGetMenu glutSetMenu

-- | Returns 'True' if the given menu identifier refers to a real menu, not
-- a pseudo one.

isRealMenu :: MenuID -> Bool
isRealMenu = (/= 0)

--------------------------------------------------------------------------------

-- | Add a menu entry to the bottom of the /current menu./ The given string will
-- be displayed for the newly added menu entry. If the menu entry is selected by
-- the user, the menu\'s callback will be called passing the given value as the
-- callback\'s parameter.

addMenuEntry :: String -> Value -> IO ()
addMenuEntry name value = withCString name $ \n -> glutAddMenuEntry n value

-- | Add a sub-menu trigger to the bottom of the /current menu./ The given
-- string will be displayed for the newly added sub-menu trigger. If the
-- sub-menu trigger is entered, the sub-menu specified by the given menu
-- identifier will be cascaded, allowing sub-menu menu items to be selected.

addSubMenu :: String -> MenuID -> IO ()
addSubMenu name menuID = withCString name $ \n -> glutAddSubMenu n menuID

--------------------------------------------------------------------------------

{- UNUSED
-- | Change the specified menu entry in the /current menu/ into a menu entry.
-- The given position determines which menu item should be changed and must be
-- between 1 (the topmost menu item) and
-- 'Graphics.UI.GLUT.State.getNumMenuItems' inclusive. The menu item to change
-- does not have to be a menu entry already. The given string will be displayed
-- for the newly changed menu entry. The given value will be returned to the
-- menu\'s callback if this menu entry is selected.

foreign import CALLCONV unsafe "glutChangeToMenuEntry" glutChangeToMenuEntry ::
   Item -> CString -> Value -> IO ()

-- | Change the specified menu item in the /current menu/ into a sub-menu
-- trigger. The  given position determines which menu item should be changed and
-- must be between 1 and 'Graphics.UI.GLUT.State.getNumMenuItems' inclusive. The
-- menu item to change does not have to be a sub-menu trigger already. The
-- given name will be displayed for the newly changed sub-menu trigger. The
-- given menu identifier names the sub-menu to cascade from the newly added
-- sub-menu trigger.

foreign import CALLCONV unsafe "glutChangeToSubMenu" glutChangeToSubMenu ::
   Item -> CString -> MenuID -> IO ()
-}

--------------------------------------------------------------------------------

-- | Attach a mouse button for the /current window/ to the identifier of the
-- /current menu./ By attaching a menu identifier to a button, the named menu
-- will be popped up when the user presses the specified button. Note that the
-- menu is attached to the button by identifier, not by reference.

attachMenu_ :: MouseButton -> IO ()
attachMenu_ = glutAttachMenu . marshalMouseButton

-- | Detach an attached mouse button from the /current window./

detachMenu_ :: MouseButton -> IO ()
detachMenu_ = glutDetachMenu . marshalMouseButton

--------------------------------------------------------------------------------

-- | Contains the number of menu items in the /current menu./

numMenuItems :: GettableStateVar Int
numMenuItems = makeGettableStateVar $ simpleGet fromIntegral glut_MENU_NUM_ITEMS

--------------------------------------------------------------------------------

setMenuFont :: MenuID -> BitmapFont -> IO ()
setMenuFont menuID font = glutSetMenuFont menuID =<< marshalBitmapFont font
