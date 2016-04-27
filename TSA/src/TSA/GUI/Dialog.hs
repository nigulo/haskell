
module TSA.GUI.Dialog (
    FitWidgets(..),
    addWidget,
    addWidgetToBox,
    addLabel,
    addSeparator,
    addFitWidgets,
    getFitParams,
    dialogWithTitle,
) where

import Graphics.UI.Gtk hiding (addWidget)
import Graphics.UI.Gtk.Layout.VBox

import TSA.GUI.State
import TSA.CommonParams
import TSA.RegressionParams
import TSA.Params
import Control.Concurrent.MVar
import Control.Applicative
import GUI.Widget

dialogWithTitle :: State -> String -> IO (Dialog)
dialogWithTitle state name =
    do
        let
            win = getWindow state
        dialog <- dialogNew
        icon <- windowGetIcon win
        dialog `set` [windowTitle := name, windowIcon := icon]
        return dialog
        
        
addWidget :: (WidgetClass w, DialogClass d) => Maybe String -> w -> d -> IO (Maybe Label, HBox)
addWidget maybeName w dialog = do
    contentBox <- castToBox <$> dialogGetContentArea dialog
    
    hBox <- hBoxNew True 0
    maybeLabel <-
        case maybeName of 
            Just name ->
                do
                    label <- labelNew maybeName
                    boxPackStart hBox label PackNatural 2
                    return $ Just label
            Nothing -> return Nothing
    boxPackEnd hBox w PackGrow 2
    boxPackStart contentBox hBox PackNatural 2
    return (maybeLabel, hBox)

addLabel :: DialogClass d => String -> d -> IO (Label, HBox)
addLabel text dialog = do
    contentBox <- castToBox <$> dialogGetContentArea dialog
    vBox <- vBoxNew False 2
    boxPackStart contentBox vBox PackGrow 2
    
    hBox <- hBoxNew True 0
    label <- labelNew (Just text)
    boxPackStart hBox label PackGrow 2
    boxPackStart vBox hBox PackNatural 2
    return (label, hBox)

addSeparator :: DialogClass d => d -> IO ()
addSeparator dialog = do
    contentBox <- castToBox <$> dialogGetContentArea dialog
    vBox <- vBoxNew False 2
    boxPackStart contentBox vBox PackGrow 2
    
    hBox <- hBoxNew True 0
    sep <- hSeparatorNew
    boxPackStart hBox sep PackGrow 2
    boxPackStart vBox hBox PackNatural 2

addWidgetToBox :: (WidgetClass w, BoxClass b) => Maybe String -> w -> Packing -> b -> IO ()
addWidgetToBox maybeName w packing box = do
    
    hBox <- hBoxNew True 0
    case maybeName of 
        Just name ->
            do
                label <- labelNew maybeName
                boxPackStart hBox label PackNatural 2
        Nothing -> return ()
    boxPackEnd hBox w PackGrow 2
    boxPackStart box hBox packing 2

data FitWidgets = FitWidgets {
    fitNameEntry :: Entry, 
    fitNumKnotsSpin :: SpinButton, 
    fitPeriodSpin :: SpinButton, 
    fitHarmonicsSpin :: SpinButton,
    fitTypeCombo :: ComboBox,
    fitNumNodesSpin :: SpinButton, 
    fitSlowHarmonicPeriodSpin :: SpinButton,
    fitSlowHarmonicsSpin :: SpinButton
}

addFitWidgets :: FitParams -> State -> Dialog -> 
    IO FitWidgets
addFitWidgets fitParams state dialog = do
    nameEntry <- entryNew
    nameEntry `entrySetText` (((commonName . fitCommonParams) fitParams) ++ (show ((commonNo . fitCommonParams) fitParams)))
    rankAdjustment <- adjustmentNew (fromIntegral (fitPolynomRank fitParams)) 1 10000 1 1 1
    harmonicsAdjustment <- adjustmentNew (fromIntegral (fitNumHarmonics fitParams)) 0 10000 1 1 1
    periodAdjustment <- adjustmentNew (fitPeriod fitParams) 0 1000000000000 1 1 1
    rankSpin <- spinButtonNew rankAdjustment  1 0
    periodSpin <- spinButtonNew periodAdjustment 1 10
    harmonicsSpin <- spinButtonNew harmonicsAdjustment 1 0

    TSA.GUI.Dialog.addWidget (Just "Name: ") nameEntry dialog
    TSA.GUI.Dialog.addWidget (Just "Polynom degree: ") rankSpin dialog
    TSA.GUI.Dialog.addWidget (Just "Period: ") periodSpin dialog
    TSA.GUI.Dialog.addWidget (Just "Num harmonics: ") harmonicsSpin dialog

    typeCombo <- createComboBox [
        "Spline", 
        "Harmonic" 
        ]
    addWidget (Just "Type: ") typeCombo dialog

    numNodesAdjustment <- adjustmentNew (fromIntegral (splineNumNodes (fitSplineParams fitParams))) 1 10000 1 1 1
    numNodesSpin <- spinButtonNew numNodesAdjustment 1 0
    (_, numNodesBox) <- addWidget (Just "Num nodes: ") numNodesSpin dialog
    
    slowHarmonicCoverageFactorAdjustment <- adjustmentNew (harmonicCoverageFactor (fitHarmonicParams fitParams)) 0 100000 1 1 1
    slowHarmonicCoverageFactorSpin <- spinButtonNew slowHarmonicCoverageFactorAdjustment 1 10
    (_, slowHarmonicCoverageFactorBox) <- addWidget (Just "Coverage factor: ") slowHarmonicCoverageFactorSpin dialog

    slowHarmonicsAdjustment <- adjustmentNew (fromIntegral (harmonicCount (fitHarmonicParams fitParams))) 0 10000 1 1 1
    slowHarmonicsSpin <- spinButtonNew slowHarmonicsAdjustment 1 0
    (_, slowHarmonicsBox) <- addWidget (Just "Num modulators: ") slowHarmonicsSpin dialog

    case fitType fitParams of
        FitTypeSpline -> do
            comboBoxSetActive typeCombo 0
            numNodesBox `set`  [widgetVisible := True]
            slowHarmonicCoverageFactorBox `set` [widgetVisible := False]
        FitTypeHarmonic -> do
            comboBoxSetActive typeCombo 1
            slowHarmonicCoverageFactorBox `set`  [widgetVisible := True]
            numNodesBox `set` [widgetVisible := False]

    let
        showFitTypeCombos =
            do
                fitType <- comboBoxGetActive typeCombo
                if fitType == 0 
                    then
                    do
                            numNodesBox `set`  [widgetVisible := True]
                            slowHarmonicCoverageFactorBox `set`  [widgetVisible := False]
                            slowHarmonicsBox `set`  [widgetVisible := False]
                    else   
                    do              
                            numNodesBox `set`  [widgetVisible := False]
                            slowHarmonicCoverageFactorBox `set`  [widgetVisible := True]
                            slowHarmonicsBox `set`  [widgetVisible := True]

    after dialog realize showFitTypeCombos
    on typeCombo changed showFitTypeCombos

    return $ FitWidgets nameEntry rankSpin periodSpin harmonicsSpin typeCombo numNodesSpin slowHarmonicCoverageFactorSpin slowHarmonicsSpin

getFitParams :: FitWidgets -> FitParams -> IO (FitParams)
getFitParams (FitWidgets nameEntry rankSpin periodSpin harmonicsSpin typeCombo numNodesSpin slowHarmonicCoverageFactorSpin slowHarmonicsSpin) fitParams = do
    name <- entryGetString nameEntry
    rank <- spinButtonGetValueAsInt rankSpin
    period <- spinButtonGetValue periodSpin
    harmonics <- spinButtonGetValueAsInt harmonicsSpin
    fitType <- comboBoxGetActive typeCombo
    numNodes <- spinButtonGetValueAsInt numNodesSpin
    slowHarmonicCoverageFactor <- spinButtonGetValue slowHarmonicCoverageFactorSpin
    slowHarmonics <- spinButtonGetValueAsInt slowHarmonicsSpin
    return fitParams {
        fitPolynomRank = rank,
        fitPeriod = period,
        fitNumHarmonics = harmonics,
        fitCommonParams = updateCommonParams name (fitCommonParams fitParams),
        fitType = if fitType == 0 then FitTypeSpline else FitTypeHarmonic,
        fitSplineParams = SplineParams numNodes, 
        fitHarmonicParams = HarmonicParams slowHarmonicCoverageFactor slowHarmonics
    }

