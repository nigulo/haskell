
module GUI.Widget (
    module System.Glib.UTFString,
    labelWithImage,
    labelWithButton,
    ItemChooser,
    itemChooserNew,
    itemChooserToWidget,
    itemChooserGetChoice,
    createComboBox,
    comboBoxGetActiveString,
    comboBoxAppendString,
    entryGetString
    ) where

import Data.List

import Graphics.UI.Gtk hiding (addWidget)
import Graphics.UI.Gtk.ModelView.ListStore
import Graphics.UI.Gtk.ModelView.TreeView
import Graphics.UI.Gtk.ModelView.TreeSelection
import Graphics.UI.Gtk.ModelView.TreeViewColumn
import System.Glib.UTFString

import Control.Monad.IO.Class

labelWithImage :: Maybe String -> Maybe StockId -> IO (HBox)
labelWithImage maybeLabelText maybeStockId =
    do
        hBox <- hBoxNew False 0
        label <- labelNew maybeLabelText
        boxPackStart hBox label PackGrow 2

        case maybeStockId of
            Just stockId -> 
                do
                    image <- imageNewFromStock stockId IconSizeMenu
                    boxPackStart hBox image PackNatural 2
            otherwise -> return ()
            
        return hBox

labelWithButton :: Maybe String -> StockId -> IO () -> IO (HBox)
labelWithButton maybeLabelText stockId onClickAction =
    do
        hBox <- hBoxNew False 0
        label <- labelNew maybeLabelText
        boxPackStart hBox label PackGrow 2

        image <- imageNewFromStock stockId IconSizeMenu
        button <- buttonNew
        buttonSetImage button image
        buttonSetRelief button ReliefNone
        on button buttonReleaseEvent $ liftIO (onClickAction >> return True)
        boxPackStart hBox button PackNatural 2
        
        --button `onEnter` (widgetShow button)
        --button `onLeave` (widgetHide button)
        
        return hBox

--------------------------------------------------------------------------------
-- | Item chooser
newtype ItemChooser = ItemChooser (Frame, ListStore String {-options-}, ListStore String {-chosen-})
 
itemChooserNew :: Maybe String -> String -> [String] -> String -> [String] -> IO ItemChooser
itemChooserNew title optionsTitle options chosenTitle chosen = 
    do

        list1 <- listStoreNew options
        treeView1 <- treeViewNewWithModel list1
        col1 <- treeViewColumnNew
        alignment1 <- alignmentNew 0 0 0 0 
        invisible1 <- invisibleNew
        containerAdd alignment1 invisible1
        treeViewColumnSetWidget col1 (Just alignment1)
        --treeViewColumnSetTitle col1 optionsTitle
        renderer1 <- cellRendererTextNew
        cellLayoutPackStart col1 renderer1 False
        cellLayoutSetAttributes col1 renderer1 list1 $ \ind -> [cellText := ind]
        treeViewAppendColumn treeView1 col1
           
        list2 <- listStoreNew chosen
        treeView2 <- treeViewNewWithModel list2
        col2 <- treeViewColumnNew
        alignment2 <- alignmentNew 0 0 0 0 
        invisible2 <- invisibleNew
        containerAdd alignment2 invisible2
        treeViewColumnSetWidget col2 (Just alignment2)
        --treeViewColumnSetTitle col2 chosenTitle
        renderer2 <- cellRendererTextNew
        cellLayoutPackStart col2 renderer2 False
        cellLayoutSetAttributes col2 renderer2 list2 $ \ind -> [cellText := ind]
        treeViewAppendColumn treeView2 col2
        
        frame <- frameNew
        case title of
            Just title -> frameSetLabel frame title
            otherwise -> return ()
        
        hBox <- hBoxNew False 0
        boxPackStart hBox treeView1 PackGrow 2
        
        buttonBox1 <- vButtonBoxNew
        buttonBoxSetLayout buttonBox1 ButtonboxCenter        
        rightButton <- buttonNewWithLabel "Add >>"
        leftButton <- buttonNewWithLabel "Remove"
        
        on rightButton buttonReleaseEvent $ liftIO $
            do
                selection1 <- treeViewGetSelection treeView1
                sel1 <- treeSelectionGetSelectedRows selection1
                if length sel1 > 0
                    then  
                        do
                            vals1 <- mapM (listStoreGetValue list1) $ (head sel1)
                            vals2 <- listStoreToList list2
                            mapM_ (listStoreAppend list2) $ vals1 \\ vals2
                            return True
                    else
                            return True
        on leftButton buttonReleaseEvent $ liftIO $
            do
                selection2 <- treeViewGetSelection treeView2
                sel2 <- treeSelectionGetSelectedRows selection2
                if length sel2 > 0  
                    then
                        do
                            mapM_ (listStoreRemove list2) $ head sel2
                            return True
                    else
                        return True
        
        boxPackStart buttonBox1 rightButton PackNatural 2
        boxPackStart buttonBox1 leftButton PackNatural 2

        boxPackStart hBox buttonBox1 PackNatural 2
        
        boxPackStart hBox treeView2 PackGrow 2
        
        buttonBox2 <- vButtonBoxNew
        buttonBoxSetLayout buttonBox2 ButtonboxCenter        
        upButton <- buttonNewWithLabel "Up"
        downButton <- buttonNewWithLabel "Down"

        on upButton buttonReleaseEvent $ liftIO $
            do
                selection <- treeViewGetSelection treeView2
                sel <- treeSelectionGetSelectedRows selection
                let selIndex = head (head sel)
                if length sel > 0 && selIndex > 0
                    then  
                        do
                            val <- listStoreGetValue list2 $ selIndex
                            listStoreRemove list2 selIndex
                            listStoreInsert list2 (selIndex - 1) val
                            treeSelectionSelectPath selection [selIndex - 1]
                            return True
                    else
                        return True
        on downButton buttonReleaseEvent $ liftIO $
            do
                selection <- treeViewGetSelection treeView2
                sel <- treeSelectionGetSelectedRows selection
                size <- listStoreGetSize list2
                let selIndex = head (head sel)
                if length sel > 0 && selIndex < size - 1
                    then  
                        do
                            val <- listStoreGetValue list2 $ selIndex
                            listStoreRemove list2 selIndex
                            listStoreInsert list2 (selIndex + 1) val
                            treeSelectionSelectPath selection [selIndex + 1]
                            return True
                    else
                        return True
        boxPackStart buttonBox2 upButton PackNatural 2
        boxPackStart buttonBox2 downButton PackNatural 2

        boxPackStart hBox buttonBox2 PackNatural 2
        
        containerAdd frame hBox
        return $ ItemChooser (frame, list1, list2)

itemChooserToWidget :: ItemChooser -> Frame
itemChooserToWidget (ItemChooser (frame, _, _)) = frame

itemChooserGetChoice :: ItemChooser -> IO [String]
itemChooserGetChoice (ItemChooser (_, _, list2)) =
    do
        choice <- listStoreToList list2
        return choice
 
createComboBox :: [String] -> IO ComboBox
createComboBox options = 
    do
        comboBox <- comboBoxNew --WithEntry
        comboBoxSetModelText comboBox
        mapM_ (\str -> comboBoxAppendText comboBox (stringToGlib str)) options
        return comboBox
        
comboBoxGetActiveString :: ComboBox -> IO (Maybe String)
comboBoxGetActiveString comboBox = do
    maybeText <- comboBoxGetActiveText comboBox
    case maybeText of
        Just text -> return $ Just $ glibToString text
        Nothing -> return Nothing
    
comboBoxAppendString :: ComboBox -> String -> IO ()
comboBoxAppendString comboBox str = do
    comboBoxAppendText comboBox (stringToGlib str)
    return ()

entryGetString :: Entry -> IO String
entryGetString entry = do
    text <- entryGetText entry
    return (glibToString text)
