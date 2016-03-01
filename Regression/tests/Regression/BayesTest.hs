{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Regression.BayesTest where

import Regression.RBF as RBF
import Regression.Bayes
import qualified Data.Eigen.Matrix as M
import Test.Framework
import Utils.Test
import Data.List
import Control.Monad
import Debug.Trace

test_bayesLinReg = do
    let
        xTrain :: [Double] = [
            3.7812, -4.7261, 1.7047, -0.8270, 0.5869, -3.5961, -3.0190, 3.0074, 4.6826, -1.8658,
            1.9232, 3.7639, 3.9461, -4.1496, -4.6095, -3.3017, 3.7814, -4.0165, -0.7889, 4.5789,
            0.3317, 1.9188, -1.8448, 1.8650, 3.3463, -4.8171, 2.5014, 4.8886, 2.4817, -2.1956,
            2.8928, -3.9677, -0.5211, 4.0860, -2.0639, -2.1222, -3.6997, -4.8063, 1.7884, -2.8837,
           -2.3445, -0.0843, -4.4664, 0.7412, -3.5327, 0.8931, 1.9976, -3.9767, -0.8594, 1.9440
           ]

        yTrain :: [Double] = [
            0.5804, -0.2090, -0.2874, -0.8108, -0.7795, 0.6245, -0.6563, 0.1730, -0.3846, -1.1996,
           -0.0588, 0.3149, 0.7521, 0.8589, 0.1093, -0.5251, 0.5377, 0.9668, -0.9194, -0.3436,
           -0.9101, 0.3604, -1.2812, -0.3467, 0.1343, -0.4037, -0.0081, -0.4374, -0.0815, -0.9747,
           -0.0395, 0.7844, -0.5342, 0.5128, -1.1994, -1.5737, 0.8318, -0.1754, -0.1772, -0.7289,
           -1.1197, -0.6862, 0.5082, -0.8036, 0.0758, -0.7829, -0.1923, 0.9154, -0.8727, 0.0770
            ]
        
        --rangeStart = -5
        --rangeEnd = 5
        --numBasisFunctions = 11
        --lambdas = repeat 3.9234
        --centres = [-5, -5 + (rangeEnd - rangeStart) / (fromIntegral numBasisFunctions - 1) .. 5]
        
        --(sumPhi, sumyPhi) = foldl' (\(sPhi, syPhi) (x, y) ->
        --        let
        --            phi = M.fromList $ map (\p -> [p]) $ RBF.values (M.fromList [[x]]) (zipWith (\c l -> ((M.fromList [[c]]), l)) centres lambdas)
        --            --phi1 = trace ("size: " ++ show (M.dims ((phi `M.mul` (M.transpose phi))))) phi
        --        in
        --            (sPhi `M.add` (phi `M.mul` (M.transpose phi)), syPhi `M.add` (M.map (*y) phi))
        --    ) (M.zero numBasisFunctions numBasisFunctions, M.zero numBasisFunctions 1) (zip xTrain yTrain)

        sumPhi :: M.MatrixXd = M.fromList $ [    
            [8.861800985766141e+00, 1.041584957232027e+01, 8.144241915551671e+00, 4.394412437913028e+00, 1.711908447701046e+00, 5.015856555041788e-01, 1.130366246262387e-01,
             1.978828615019784e-02, 2.726829458177522e-03, 3.007599820220453e-04, 2.692962023770108e-05],
            [1.041584957232027e+01, 1.355922681964368e+01, 1.218061834902503e+01, 7.900113229057926e+00, 3.853737677552452e+00, 1.445907405494973e+00, 4.214186423270310e-01,
             9.668248381303526e-02, 1.775391484481706e-02, 2.646601605436716e-03, 3.230891950177113e-04],
            [8.144241915551671e+00, 1.218061834902503e+01, 1.315278061290350e+01, 1.068195316910614e+01, 6.672571910889041e+00, 3.237805711023070e+00, 1.236713496967476e+00,
             3.780939204690915e-01, 9.383792452066503e-02, 1.907201223068462e-02, 3.137839019931413e-03],
            [4.394412437913028e+00, 7.900113229057926e+00, 1.068195316910614e+01, 1.110906539224519e+01, 8.974686880550422e+00, 5.707184090987924e+00, 2.904937114879513e+00,
             1.200327331334811e+00, 4.061646086828642e-01, 1.112552416296651e-01, 2.368328387723070e-02],
            [1.711908447701046e+00, 3.853737677552452e+00, 6.672571910889041e+00, 8.974686880550422e+00, 9.501805618445372e+00, 8.052027620117874e+00, 5.539269253687286e+00,
             3.120607295270749e+00, 1.423120854009689e+00, 5.043679508994995e-01, 1.328779648402726e-01],
            [5.015856555041788e-01, 1.445907405494973e+00, 3.237805711023070e+00, 5.707184090987924e+00, 8.052027620117874e+00, 9.222246711802965e+00, 8.649831352409020e+00,
             6.567416558057381e+00, 3.875114358638458e+00, 1.699707807314096e+00, 5.449999522898860e-01],
            [1.130366246262387e-01, 4.214186423270310e-01, 1.236713496967476e+00, 2.904937114879513e+00, 5.539269253687286e+00, 8.649831352409020e+00, 1.093399381466576e+01,
             1.074120595831432e+01, 7.843809727166009e+00, 4.187294487703555e+00, 1.654467030035393e+00],
            [1.978828615019784e-02, 9.668248381303526e-02, 3.780939204690915e-01, 1.200327331334811e+00, 3.120607295270749e+00, 6.567416558057381e+00, 1.074120595831432e+01,
             1.305904175288327e+01, 1.160652005024721e+01, 7.635032637741449e+00, 3.764519770586256e+00],
            [2.726829458177522e-03, 1.775391484481706e-02, 9.383792452066503e-02, 4.061646086828642e-01, 1.423120854009689e+00, 3.875114358638458e+00, 7.843809727166009e+00,
             1.160652005024721e+01, 1.271145189251246e+01, 1.043465519923917e+01, 6.319534763808286e+00],
            [3.007599820220453e-04, 2.646601605436716e-03, 1.907201223068462e-02, 1.112552416296651e-01, 5.043679508994995e-01, 1.699707807314096e+00, 4.187294487703555e+00,
             7.635032637741449e+00, 1.043465519923917e+01, 1.052129911483549e+01, 7.521388990417746e+00],
            [2.692962023770108e-05, 3.230891950177113e-04, 3.137839019931413e-03, 2.368328387723070e-02, 1.328779648402726e-01, 5.449999522898860e-01, 1.654467030035393e+00,
             3.764519770586256e+00, 6.319534763808286e+00, 7.521388990417746e+00, 6.090901536358487e+00]]
        
        sumyPhi :: M.MatrixXd = M.fromList $ map (\y -> [y]) [
            1.926952010566491e+00,
            2.992502819453232e-01,
            -4.778635078091193e+00,
            -9.706188670547188e+00,
            -1.093755223952117e+01,
            -9.104159125259280e+00,
            -5.969759373330822e+00,
            -2.411129659391033e+00,
            4.614239924312551e-01,
            1.452061610541205e+00,
            8.463457845768559e-01]

    (m, s, alpha, beta, margLik) <- bayesLinReg (M.fromList (map (\y -> [y]) yTrain)) sumPhi sumyPhi (50, 0.001)

    let    
        mExp :: [Double] = [
            -9.631235345721318,
            16.915351130261943,
            -11.797829332650178,
            -0.4737318295008315,
            7.723824922950618,
            -8.657629968817867,
            2.8528711934575313,
            4.009457364199889,
            -9.339547006524327,
            10.80159638654996,
            -6.1486967709918545]

        sExp :: [[Double]] = [
            [2.5675153001445237,-5.586544008719154,6.776250726810027,-5.580112134614858,3.0241794758197145,-0.5801661780485232,
            -0.8741982434655092,1.2681397520756046,-1.006665561096348,0.5420670306371989,-0.16680678810405256],
            [-5.586544008719372,12.59806121784021,-15.990216804810814,14.07286927552546,-8.69448065730262,2.9977987106542985,
            0.871062273421361,-2.4277532339285726,2.3149616223129654,-1.430497927829684,0.5027654173977032],
            [6.776250726810743,-15.990216804811942,21.62390997647483,-20.92680174468085,15.2373263684466,-7.915423609657149,
            1.8506344763543492,1.656423457724653,-2.7307987700324508,2.133686795807166,-0.8809953551023942],
            [-5.580112134616772,14.072869275529285,-20.92680174468433,23.23234327465362,-20.62630395155533,14.663914957390185,
            -7.756847229335079,2.0615157586126203,1.2421036034360207,-1.981841874539368,1.048956055564843],
            [3.0241794758226495,-8.694480657309132,15.23732636845424,-20.626303951560306,22.907725013698542,-20.897940619590777,
            15.287395662665592,-8.375471548082409,2.68761325252566,0.2437198901839015,-0.6224576667120276],
            [-0.5801661780521415,2.9977987106627366,-7.915423609668044,14.663914957399493,-20.89794061959567,23.651703522464434,
            -21.514567616581893,15.653791549578761,-8.837998085334675,3.5459901082437635,-0.753418650065649],
            [-0.8741982434617911,0.8710622734126335,1.8506344763659872,-7.756847229346014,15.287395662673129,-21.514567616585285,
            23.839059063529383,-21.480175679698057,15.760574356094713,-8.8656832431688,3.01677088467401],
            [1.2681397520737012,-2.4277532339238252,1.656423457717743,2.061515758620053,-8.375471548088763,15.653791549583065,
            -21.480175679700046,23.56049627613538,-20.80280626417071,13.790306728152409,-5.3987707658526976],
            [-1.006665561095048,2.314961622309832,-2.73079877002796,1.2421036034310458,2.6876132525303933,-8.83799808533866,
            15.760574356097488,-20.802806264172,21.14452202383689,-15.56230817422873,6.58345596377697],
            [0.5420670306361267,-1.4304979278272085,2.133686795803825,-1.9818418745359183,0.24371989018075557,3.545990108246531,
            -8.865683243171047,13.790306728153853,-15.562308174229296,12.333941843581,-5.514739715191596],
            [-0.16680678810405247,0.5027654173976714,-0.880995355102251,1.0489560555644715,-0.6224576667113478,-0.7534186500665825,
            3.016770884674974,-5.39877076585343,6.583455963777351,-5.514739715191704,2.5805642801367448]]
        alphaExp :: Double = 9.919940810785056e-3
        betaExp :: Double = 24.287131711024514
        margLikExp :: Double = 8.836405741977161e7

        {-
        mExp :: [Double] = [
            -9.630835099605974e+00,
             1.691452431619830e+01,
            -1.179694765746228e+01,
            -4.742360841571092e-01,
             7.723816637390071e+00,
            -8.657370918035827e+00,
             2.852791452729768e+00,
             4.009093789027943e+00,
            -9.338831797680854e+00,
             1.080089922411304e+01,
            -6.148352922768289e+00]
        sExp :: [[Double]] = [
            [2.567627784229887e+00, -5.586713527643323e+00, 6.776313531714349e+00, -5.579954172148382e+00, 3.023832537183668e+00, -5.797838428857576e-01,
             -8.744557588013055e-01, 1.268203700551511e+00, -1.006579669939857e+00, 5.419431397378496e-01, -1.667397721929225e-01],
            [-5.586713527643323e+00, 1.259830678608423e+01, -1.599024555572020e+01, 1.407245094792205e+01, -8.693656783466979e+00, 2.996872674965694e+00,
             8.717442580070089e-01, -2.428018156388192e+00, 2.314876957756427e+00, -1.430290146310240e+00, 5.026416840541941e-01],
            [6.776313531714349e+00, -1.599024555572020e+01, 2.162367090529908e+01, -2.092608911828551e+01, 1.523616174316306e+01, -7.914101937701099e+00,
             1.849543023711361e+00, 1.657042275627383e+00, -2.730961668680710e+00, 2.133609931213345e+00, -8.809129078235243e-01],
            [-5.579954172148382e+00, 1.407245094792205e+01, -2.092608911828551e+01, 2.323132426789907e+01, -2.062500581945034e+01, 1.466246974781835e+01,
             -7.755485577304853e+00, 2.060459113617135e+00, 1.242758233029827e+00, -1.982142033624539e+00, 1.049035294636869e+00],
            [3.023832537183668e+00, -8.693656783466979e+00, 1.523616174316306e+01, -2.062500581945034e+01, 2.290638825191797e+01, -2.089653949277371e+01,
             1.528592586099944e+01, -8.374046166080673e+00, 2.686428227268085e+00, 2.444802358681697e-01, -6.227494681894821e-01],
            [-5.797838428857576e-01, 2.996872674965694e+00, -7.914101937701099e+00, 1.466246974781835e+01, -2.089653949277371e+01, 2.365031941919220e+01,
             -2.151308981832344e+01, 1.565222134502448e+01, -8.836538494850087e+00, 3.544956609559332e+00, -7.529894944481285e-01],
            [-8.744557588013055e-01, 8.717442580070089e-01, 1.849543023711361e+00, -7.755485577304853e+00, 1.528592586099944e+01, -2.151308981832344e+01,
             2.383758702943990e+01, -2.147871908539214e+01, 1.575926181771001e+01, -8.864753487068754e+00, 3.016379205822959e+00],
            [1.268203700551511e+00, -2.428018156388192e+00, 1.657042275627383e+00, 2.060459113617135e+00, -8.374046166080673e+00, 1.565222134502448e+01,
             -2.147871908539214e+01, 2.355932122444804e+01, -2.080197240503105e+01, 1.378981622819085e+01, -5.398586342503858e+00],
            [-1.006579669939857e+00, 2.314876957756427e+00, -2.730961668680710e+00, 1.242758233029827e+00, 2.686428227268085e+00, -8.836538494850087e+00,
             1.575926181771001e+01, -2.080197240503105e+01, 2.114421513172088e+01, -1.556231799233178e+01, 6.583520322013085e+00],
            [5.419431397378496e-01, -1.430290146310240e+00, 2.133609931213345e+00, -1.982142033624539e+00, 2.444802358681697e-01, 3.544956609559332e+00,
              -8.864753487068754e+00, 1.378981622819085e+01, -1.556231799233178e+01, 1.233420915959066e+01, -5.514931821203610e+00],
            [-1.667397721929225e-01, 5.026416840541941e-01, -8.809129078235243e-01, 1.049035294636869e+00, -6.227494681894821e-01, -7.529894944481285e-01,
             3.016379205822959e+00, -5.398586342503858e+00, 6.583520322013085e+00, -5.514931821203610e+00, 2.580695903146897e+00]]
        alphaExp :: Double = 9.920860653153174e-03
        betaExp :: Double = 2.428282065103534e+01
        margLikExp :: Double = 8.831700932033677e+07
        -}
        
    assertEqualDoubleList mExp (map (\[x] -> x) (M.toList m))
    zipWithM_  (assertEqualDoubleList) sExp (M.toList s)
    assertEqualDouble alphaExp alpha
    assertEqualDouble betaExp beta
    assertEqualDouble margLikExp margLik

            