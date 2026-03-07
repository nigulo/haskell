{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module TSA.GUI.Dialog (
    FitWidgets(..),
    addWidget,
    addWidgetToBox,
    addLabel,
    addSeparator,
    addFitWidgets,
    getFitParams,
    dialogWithTitle,
    runDialog,
    createComboBox,
    comboBoxGetActive,
    comboBoxSetActive,
    spinButtonGetValueAsInt,
    spinButtonGetValue,
    entryGetString,
    entrySetText,
) where

import qualified GI.Gtk as Gtk
import qualified GI.Gio as Gio
import Data.GI.Base
import Data.Text (Text)
import qualified Data.Text as T
import Data.Int (Int32)
import Data.Word (Word32)
import Control.Monad (when)
import Control.Monad.IO.Class

import TSA.GUI.State
import TSA.CommonParams
import TSA.RegressionParams
import TSA.Params
import Control.Concurrent.MVar
import GUI.Widget

-- | Create a dialog window with title
dialogWithTitle :: State -> String -> IO Gtk.Window
dialogWithTitle state name = do
    let win = getWindow state
    dialog <- Gtk.windowNew
    Gtk.windowSetTitle dialog (T.pack name)
    Gtk.windowSetModal dialog True
    Gtk.windowSetTransientFor dialog (Just win)
    Gtk.windowSetDestroyWithParent dialog True
    return dialog

-- | Run a dialog and return True if OK was clicked
runDialog :: Gtk.Window -> IO () -> IO () -> IO ()
runDialog dialog okAction cancelAction = do
    -- Create content area
    vBox <- Gtk.boxNew Gtk.OrientationVertical 4
    Gtk.widgetSetMarginTop vBox 8
    Gtk.widgetSetMarginBottom vBox 8
    Gtk.widgetSetMarginStart vBox 8
    Gtk.widgetSetMarginEnd vBox 8

    -- Button box
    buttonBox <- Gtk.boxNew Gtk.OrientationHorizontal 4
    Gtk.widgetSetHalign buttonBox Gtk.AlignEnd

    cancelButton <- Gtk.buttonNewWithLabel "Cancel"
    okButton <- Gtk.buttonNewWithLabel "OK"

    Gtk.boxAppend buttonBox cancelButton
    Gtk.boxAppend buttonBox okButton

    _ <- Gtk.onButtonClicked cancelButton $ do
        cancelAction
        Gtk.windowDestroy dialog

    _ <- Gtk.onButtonClicked okButton $ do
        okAction
        Gtk.windowDestroy dialog

    Gtk.boxAppend vBox buttonBox
    Gtk.windowSetChild dialog (Just vBox)
    Gtk.windowPresent dialog

-- | Add a widget with optional label to a box
addWidget :: Gtk.IsWidget w => Maybe String -> w -> Gtk.Box -> IO Gtk.Box
addWidget maybeName w box = do
    hBox <- Gtk.boxNew Gtk.OrientationHorizontal 4
    case maybeName of
        Just name -> do
            label <- Gtk.labelNew (Just $ T.pack name)
            Gtk.widgetSetHalign label Gtk.AlignStart
            Gtk.boxAppend hBox label
        Nothing -> return ()
    widget <- Gtk.toWidget w
    Gtk.widgetSetHexpand widget True
    Gtk.boxAppend hBox widget
    Gtk.boxAppend box hBox
    return hBox

-- | Add a widget to a box with optional label
addWidgetToBox :: Gtk.IsWidget w => Maybe String -> w -> Gtk.Box -> IO ()
addWidgetToBox maybeName w box = do
    _ <- addWidget maybeName w box
    return ()

-- | Add a label to a box
addLabel :: String -> Gtk.Box -> IO Gtk.Label
addLabel text box = do
    label <- Gtk.labelNew (Just $ T.pack text)
    Gtk.boxAppend box label
    return label

-- | Add a separator to a box
addSeparator :: Gtk.Box -> IO ()
addSeparator box = do
    sep <- Gtk.separatorNew Gtk.OrientationHorizontal
    Gtk.boxAppend box sep

-- | Create a combo box (dropdown) with string options
createComboBox :: [String] -> IO Gtk.DropDown
createComboBox options = do
    Gtk.dropDownNewFromStrings (map T.pack options)

-- | Get active index from dropdown
comboBoxGetActive :: Gtk.DropDown -> IO Int
comboBoxGetActive dropdown = do
    idx <- Gtk.dropDownGetSelected dropdown
    return $ fromIntegral idx

-- | Set active index on dropdown
comboBoxSetActive :: Gtk.DropDown -> Int -> IO ()
comboBoxSetActive dropdown idx =
    Gtk.dropDownSetSelected dropdown (fromIntegral idx)

-- | Get value as Int from SpinButton
spinButtonGetValueAsInt :: Gtk.SpinButton -> IO Int
spinButtonGetValueAsInt spin = do
    val <- Gtk.spinButtonGetValueAsInt spin
    return $ fromIntegral val

-- | Get value as Double from SpinButton
spinButtonGetValue :: Gtk.SpinButton -> IO Double
spinButtonGetValue = Gtk.spinButtonGetValue

-- | Get text from Entry
entryGetString :: Gtk.Entry -> IO String
entryGetString entry = do
    text <- GUI.Widget.entryGetString entry
    return $ T.unpack text

-- | Set text on Entry
entrySetText :: Gtk.Entry -> String -> IO ()
entrySetText entry text = do
    buffer <- Gtk.entryGetBuffer entry
    Gtk.entryBufferSetText buffer (T.pack text) (-1)

data FitWidgets = FitWidgets {
    fitNameEntry :: Gtk.Entry,
    fitNumKnotsSpin :: Gtk.SpinButton,
    fitPeriodSpin :: Gtk.SpinButton,
    fitHarmonicsSpin :: Gtk.SpinButton,
    fitTypeCombo :: Gtk.DropDown,
    fitNumNodesSpin :: Gtk.SpinButton,
    fitSlowHarmonicPeriodSpin :: Gtk.SpinButton,
    fitSlowHarmonicsSpin :: Gtk.SpinButton
}

addFitWidgets :: FitParams -> State -> Gtk.Box -> IO FitWidgets
addFitWidgets fitParams state box = do
    nameEntry <- Gtk.entryNew
    entrySetText nameEntry (((commonName . fitCommonParams) fitParams) ++ (show ((commonNo . fitCommonParams) fitParams)))

    rankAdjustment <- Gtk.adjustmentNew (fromIntegral (fitPolynomRank fitParams)) 1 10000 1 1 1
    harmonicsAdjustment <- Gtk.adjustmentNew (fromIntegral (fitNumHarmonics fitParams)) 0 10000 1 1 1
    periodAdjustment <- Gtk.adjustmentNew (fitPeriod fitParams) 0 1000000000000 1 1 1

    rankSpin <- Gtk.spinButtonNew (Just rankAdjustment) 1 0
    periodSpin <- Gtk.spinButtonNew (Just periodAdjustment) 1 10
    harmonicsSpin <- Gtk.spinButtonNew (Just harmonicsAdjustment) 1 0

    _ <- addWidget (Just "Name: ") nameEntry box
    _ <- addWidget (Just "Polynom degree: ") rankSpin box
    _ <- addWidget (Just "Period: ") periodSpin box
    _ <- addWidget (Just "Num harmonics: ") harmonicsSpin box

    typeCombo <- createComboBox ["Spline", "Harmonic"]
    _ <- addWidget (Just "Type: ") typeCombo box

    numNodesAdjustment <- Gtk.adjustmentNew (fromIntegral (splineNumNodes (fitSplineParams fitParams))) 1 10000 1 1 1
    numNodesSpin <- Gtk.spinButtonNew (Just numNodesAdjustment) 1 0
    numNodesBox <- addWidget (Just "Num nodes: ") numNodesSpin box

    slowHarmonicCoverageFactorAdjustment <- Gtk.adjustmentNew (harmonicCoverageFactor (fitHarmonicParams fitParams)) 0 100000 1 1 1
    slowHarmonicCoverageFactorSpin <- Gtk.spinButtonNew (Just slowHarmonicCoverageFactorAdjustment) 1 10
    slowHarmonicCoverageFactorBox <- addWidget (Just "Coverage factor: ") slowHarmonicCoverageFactorSpin box

    slowHarmonicsAdjustment <- Gtk.adjustmentNew (fromIntegral (harmonicCount (fitHarmonicParams fitParams))) 0 10000 1 1 1
    slowHarmonicsSpin <- Gtk.spinButtonNew (Just slowHarmonicsAdjustment) 1 0
    slowHarmonicsBox <- addWidget (Just "Num modulators: ") slowHarmonicsSpin box

    case fitType fitParams of
        FitTypeSpline -> do
            comboBoxSetActive typeCombo 0
            Gtk.widgetSetVisible numNodesBox True
            Gtk.widgetSetVisible slowHarmonicCoverageFactorBox False
            Gtk.widgetSetVisible slowHarmonicsBox False
        FitTypeHarmonic -> do
            comboBoxSetActive typeCombo 1
            Gtk.widgetSetVisible slowHarmonicCoverageFactorBox True
            Gtk.widgetSetVisible slowHarmonicsBox True
            Gtk.widgetSetVisible numNodesBox False

    -- Connect to dropdown selection change
    _ <- Gtk.onDropDownNotifySelected typeCombo $ \_ -> do
        fitTypeIdx <- comboBoxGetActive typeCombo
        if fitTypeIdx == 0
            then do
                Gtk.widgetSetVisible numNodesBox True
                Gtk.widgetSetVisible slowHarmonicCoverageFactorBox False
                Gtk.widgetSetVisible slowHarmonicsBox False
            else do
                Gtk.widgetSetVisible numNodesBox False
                Gtk.widgetSetVisible slowHarmonicCoverageFactorBox True
                Gtk.widgetSetVisible slowHarmonicsBox True

    return $ FitWidgets nameEntry rankSpin periodSpin harmonicsSpin typeCombo numNodesSpin slowHarmonicCoverageFactorSpin slowHarmonicsSpin

getFitParams :: FitWidgets -> FitParams -> IO FitParams
getFitParams (FitWidgets nameEntry rankSpin periodSpin harmonicsSpin typeCombo numNodesSpin slowHarmonicCoverageFactorSpin slowHarmonicsSpin) fitParams = do
    name <- entryGetString nameEntry
    rank <- spinButtonGetValueAsInt rankSpin
    period <- spinButtonGetValue periodSpin
    harmonics <- spinButtonGetValueAsInt harmonicsSpin
    fitTypeIdx <- comboBoxGetActive typeCombo
    numNodes <- spinButtonGetValueAsInt numNodesSpin
    slowHarmonicCoverageFactor <- spinButtonGetValue slowHarmonicCoverageFactorSpin
    slowHarmonics <- spinButtonGetValueAsInt slowHarmonicsSpin
    return fitParams {
        fitPolynomRank = rank,
        fitPeriod = period,
        fitNumHarmonics = harmonics,
        fitCommonParams = updateCommonParams name (fitCommonParams fitParams),
        fitType = if fitTypeIdx == 0 then FitTypeSpline else FitTypeHarmonic,
        fitSplineParams = SplineParams numNodes,
        fitHarmonicParams = HarmonicParams slowHarmonicCoverageFactor slowHarmonics
    }
