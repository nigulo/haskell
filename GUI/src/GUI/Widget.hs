{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module GUI.Widget (
    labelWithImage,
    labelWithButton,
    ItemChooser,
    itemChooserNew,
    itemChooserGetWidget,
    itemChooserGetChoice,
    createDropDown,
    dropDownGetActiveString,
    entryGetString
    ) where

import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.GI.Base
import Data.Word (Word32)
import Data.Int (Int32)

import qualified GI.Gtk as Gtk
import qualified GI.Gio as Gio
import qualified GI.GObject as GObject

import Control.Monad.IO.Class
import Control.Monad (forM_, when)

labelWithImage :: Maybe Text -> Maybe Text -> IO Gtk.Box
labelWithImage maybeLabelText maybeIconName = do
    hBox <- Gtk.boxNew Gtk.OrientationHorizontal 0

    case maybeLabelText of
        Just labelText -> do
            label <- Gtk.labelNew (Just labelText)
            Gtk.boxAppend hBox label
        Nothing -> return ()

    case maybeIconName of
        Just iconName -> do
            image <- Gtk.imageNewFromIconName (Just iconName)
            Gtk.boxAppend hBox image
        Nothing -> return ()

    return hBox

labelWithButton :: Maybe Text -> Text -> IO () -> IO Gtk.Box
labelWithButton maybeLabelText iconName onClickAction = do
    hBox <- Gtk.boxNew Gtk.OrientationHorizontal 0

    case maybeLabelText of
        Just labelText -> do
            label <- Gtk.labelNew (Just labelText)
            Gtk.widgetSetHexpand label True
            Gtk.boxAppend hBox label
        Nothing -> return ()

    image <- Gtk.imageNewFromIconName (Just iconName)
    button <- Gtk.buttonNew
    Gtk.buttonSetChild button (Just image)
    Gtk.buttonSetHasFrame button False

    _ <- Gtk.onButtonClicked button onClickAction
    Gtk.boxAppend hBox button

    return hBox

--------------------------------------------------------------------------------
-- | Item chooser using ListBox (GTK4 replacement for TreeView in simple cases)
data ItemChooser = ItemChooser
    { itemChooserFrame :: Gtk.Frame
    , itemChooserOptionsList :: Gtk.StringList
    , itemChooserChosenList :: Gtk.StringList
    , itemChooserOptionsListBox :: Gtk.ListBox
    , itemChooserChosenListBox :: Gtk.ListBox
    }

itemChooserNew :: Maybe Text -> Text -> [Text] -> Text -> [Text] -> IO ItemChooser
itemChooserNew title optionsTitle options chosenTitle chosen = do
    -- Create string lists for options and chosen items
    optionsList <- Gtk.stringListNew (Just options)
    chosenList <- Gtk.stringListNew (Just chosen)

    -- Create list boxes
    optionsListBox <- Gtk.listBoxNew
    Gtk.listBoxBindModel optionsListBox (Just optionsList) (Just createListBoxRow)
    Gtk.widgetSetVexpand optionsListBox True

    chosenListBox <- Gtk.listBoxNew
    Gtk.listBoxBindModel chosenListBox (Just chosenList) (Just createListBoxRow)
    Gtk.widgetSetVexpand chosenListBox True

    -- Wrap in scrolled windows
    optionsScroll <- Gtk.scrolledWindowNew
    Gtk.scrolledWindowSetChild optionsScroll (Just optionsListBox)
    Gtk.widgetSetVexpand optionsScroll True
    Gtk.widgetSetHexpand optionsScroll True

    chosenScroll <- Gtk.scrolledWindowNew
    Gtk.scrolledWindowSetChild chosenScroll (Just chosenListBox)
    Gtk.widgetSetVexpand chosenScroll True
    Gtk.widgetSetHexpand chosenScroll True

    -- Create frame
    frame <- Gtk.frameNew title

    -- Main horizontal box
    hBox <- Gtk.boxNew Gtk.OrientationHorizontal 4
    Gtk.boxAppend hBox optionsScroll

    -- Button box for Add/Remove
    buttonBox1 <- Gtk.boxNew Gtk.OrientationVertical 4
    Gtk.widgetSetValign buttonBox1 Gtk.AlignCenter

    rightButton <- Gtk.buttonNewWithLabel "Add >>"
    leftButton <- Gtk.buttonNewWithLabel "Remove"

    _ <- Gtk.onButtonClicked rightButton $ do
        maybeRow <- Gtk.listBoxGetSelectedRow optionsListBox
        case maybeRow of
            Just row -> do
                idx <- Gtk.listBoxRowGetIndex row
                when (idx >= 0) $ do
                    maybeItem <- Gio.listModelGetItem optionsList (fromIntegral idx)
                    case maybeItem of
                        Just item -> do
                            strObj <- GObject.unsafeCastTo Gtk.StringObject item
                            str <- Gtk.stringObjectGetString strObj
                            -- Check if already in chosen list
                            n <- Gio.listModelGetNItems chosenList
                            alreadyExists <- checkStringInList chosenList str (fromIntegral n)
                            when (not alreadyExists) $ do
                                Gtk.stringListAppend chosenList str
                        Nothing -> return ()
            Nothing -> return ()

    _ <- Gtk.onButtonClicked leftButton $ do
        maybeRow <- Gtk.listBoxGetSelectedRow chosenListBox
        case maybeRow of
            Just row -> do
                idx <- Gtk.listBoxRowGetIndex row
                when (idx >= 0) $ do
                    Gtk.stringListRemove chosenList (fromIntegral idx)
            Nothing -> return ()

    Gtk.boxAppend buttonBox1 rightButton
    Gtk.boxAppend buttonBox1 leftButton
    Gtk.boxAppend hBox buttonBox1

    Gtk.boxAppend hBox chosenScroll

    -- Button box for Up/Down
    buttonBox2 <- Gtk.boxNew Gtk.OrientationVertical 4
    Gtk.widgetSetValign buttonBox2 Gtk.AlignCenter

    upButton <- Gtk.buttonNewWithLabel "Up"
    downButton <- Gtk.buttonNewWithLabel "Down"

    _ <- Gtk.onButtonClicked upButton $ do
        maybeRow <- Gtk.listBoxGetSelectedRow chosenListBox
        case maybeRow of
            Just row -> do
                idx <- Gtk.listBoxRowGetIndex row
                when (idx > 0) $ do
                    maybeItem <- Gio.listModelGetItem chosenList (fromIntegral idx)
                    case maybeItem of
                        Just item -> do
                            strObj <- GObject.unsafeCastTo Gtk.StringObject item
                            str <- Gtk.stringObjectGetString strObj
                            Gtk.stringListRemove chosenList (fromIntegral idx)
                            -- GTK4 StringList doesn't have insert, so we need to rebuild
                            -- For simplicity, we splice at position
                            Gtk.stringListSplice chosenList (fromIntegral (idx - 1)) 0 (Just [str])
                            -- Reselect the moved item
                            newRow <- Gtk.listBoxGetRowAtIndex chosenListBox (idx - 1)
                            case newRow of
                                Just r -> Gtk.listBoxSelectRow chosenListBox (Just r)
                                Nothing -> return ()
                        Nothing -> return ()
            Nothing -> return ()

    _ <- Gtk.onButtonClicked downButton $ do
        maybeRow <- Gtk.listBoxGetSelectedRow chosenListBox
        case maybeRow of
            Just row -> do
                idx <- Gtk.listBoxRowGetIndex row
                n <- Gio.listModelGetNItems chosenList
                when (idx >= 0 && fromIntegral idx < n - 1) $ do
                    maybeItem <- Gio.listModelGetItem chosenList (fromIntegral idx)
                    case maybeItem of
                        Just item -> do
                            strObj <- GObject.unsafeCastTo Gtk.StringObject item
                            str <- Gtk.stringObjectGetString strObj
                            Gtk.stringListRemove chosenList (fromIntegral idx)
                            Gtk.stringListSplice chosenList (fromIntegral (idx + 1)) 0 (Just [str])
                            -- Reselect the moved item
                            newRow <- Gtk.listBoxGetRowAtIndex chosenListBox (idx + 1)
                            case newRow of
                                Just r -> Gtk.listBoxSelectRow chosenListBox (Just r)
                                Nothing -> return ()
                        Nothing -> return ()
            Nothing -> return ()

    Gtk.boxAppend buttonBox2 upButton
    Gtk.boxAppend buttonBox2 downButton
    Gtk.boxAppend hBox buttonBox2

    Gtk.frameSetChild frame (Just hBox)

    return $ ItemChooser frame optionsList chosenList optionsListBox chosenListBox

-- Helper to create a row widget from a StringObject
createListBoxRow :: GObject.Object -> IO Gtk.Widget
createListBoxRow obj = do
    strObj <- GObject.unsafeCastTo Gtk.StringObject obj
    str <- Gtk.stringObjectGetString strObj
    label <- Gtk.labelNew (Just str)
    Gtk.widgetSetHalign label Gtk.AlignStart
    Gtk.toWidget label

-- Helper to check if string exists in StringList
checkStringInList :: Gtk.StringList -> Text -> Word32 -> IO Bool
checkStringInList _ _ 0 = return False
checkStringInList list str n = do
    maybeItem <- Gio.listModelGetItem list (n - 1)
    case maybeItem of
        Just item -> do
            strObj <- GObject.unsafeCastTo Gtk.StringObject item
            itemStr <- Gtk.stringObjectGetString strObj
            if itemStr == str
                then return True
                else checkStringInList list str (n - 1)
        Nothing -> checkStringInList list str (n - 1)

itemChooserGetWidget :: ItemChooser -> Gtk.Frame
itemChooserGetWidget = itemChooserFrame

itemChooserGetChoice :: ItemChooser -> IO [Text]
itemChooserGetChoice chooser = do
    let list = itemChooserChosenList chooser
    n <- Gio.listModelGetNItems list
    getStrings list 0 (fromIntegral n)
  where
    getStrings :: Gtk.StringList -> Word32 -> Word32 -> IO [Text]
    getStrings _ idx total | idx >= total = return []
    getStrings list idx total = do
        maybeItem <- Gio.listModelGetItem list idx
        case maybeItem of
            Just item -> do
                strObj <- GObject.unsafeCastTo Gtk.StringObject item
                str <- Gtk.stringObjectGetString strObj
                rest <- getStrings list (idx + 1) total
                return (str : rest)
            Nothing -> getStrings list (idx + 1) total

-- | Create a dropdown (GTK4 replacement for ComboBox)
createDropDown :: [Text] -> IO Gtk.DropDown
createDropDown options = do
    dropDown <- Gtk.dropDownNewFromStrings options
    return dropDown

dropDownGetActiveString :: Gtk.DropDown -> IO (Maybe Text)
dropDownGetActiveString dropDown = do
    idx <- Gtk.dropDownGetSelected dropDown
    if idx == maxBound  -- GTK_INVALID_LIST_POSITION
        then return Nothing
        else do
            maybeModel <- Gtk.dropDownGetModel dropDown
            case maybeModel of
                Just model -> do
                    maybeItem <- Gio.listModelGetItem model idx
                    case maybeItem of
                        Just item -> do
                            strObj <- GObject.unsafeCastTo Gtk.StringObject item
                            str <- Gtk.stringObjectGetString strObj
                            return (Just str)
                        Nothing -> return Nothing
                Nothing -> return Nothing

entryGetString :: Gtk.Entry -> IO Text
entryGetString entry = do
    buffer <- Gtk.entryGetBuffer entry
    Gtk.entryBufferGetText buffer
