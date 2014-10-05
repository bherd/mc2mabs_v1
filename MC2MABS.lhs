{-# LANGUAGE ForeignFunctionInterface #-}

> module Main where

> import Foreign
> import Foreign.C
> import Foreign.C.Types

> import Text.Printf
> import Debug.Trace
> import Data.List
> import System.Random
> import Control.Applicative
> import Control.Monad.Reader
> import Control.Monad.State
> import Control.DeepSeq
> import qualified Data.Vector as V

> import ModelChecker
> import SimLTL

> foreign import ccall "abs.h getProperty"
>   c_getProperty :: IO (CString)
> foreign import ccall "abs.h getFormula"
>   c_getFormula :: CInt -> IO (CString)

> foreign import ccall "abs.h preConf" 
>   c_preConf :: CInt -> IO ()
> foreign import ccall "abs.h preRun" 
>   c_preRun :: CInt -> IO ()
> foreign import ccall "abs.h step" 
>   c_step :: CInt -> CInt -> IO (Ptr (Ptr (Ptr (CInt))))
> foreign import ccall "abs.h postRun" 
>   c_tearDownRun :: CInt -> IO ()
> foreign import ccall "abs.h postConf" 
>   c_tearDownConf :: CInt -> IO ()

> foreign import ccall "abs.h run" 
>   c_run :: IO (Ptr (Ptr (Ptr (Ptr (CInt)))))

> foreign import ccall "abs.h writeGLHToFile" 
>   c_writeGLHToFile :: CInt -> IO ()

///////////////////////////////////////////////////////////////////
/// DATA TYPES
///////////////////////////////////////////////////////////////////

> data Conf = Pr {
>                  pConfiguration :: Int,
>                  pConventional :: Bool, 
>                  pFragmentSize :: Int,
>                  pNumAgents :: Int,
>                  pNumAtts :: Int,
>                  pNumTicks :: Int,
>                  pNumReps :: Int
>                }
>             deriving (Eq,Read,Show)

> data Prop = Atom Conf
>             | Div Prop Prop
>             | Mult Prop Prop
>             | Add Prop Prop
>             | Sub Prop Prop
>            deriving (Eq,Read,Show)

> data CheckEnv = CheckEnv {
>                   rNumAgents  :: Int,
>                   rTicks      :: [Int],
>                   rNumAtts    :: Int,
>                   rObligation :: GObligation,
>                   rTick       :: Int,
>                   rConfId     :: Int
>                 }

///////////////////////////////////////////////////////////////////
/// HELPER FUNCTIONS
///////////////////////////////////////////////////////////////////

Returns a number of tuples each comprising a sublist of given length and the start index of the sublist
param 1: input list
param 2: desired length of sublists

> getSublists :: [a] -> Int -> IO [([a],Int)]
> getSublists xs l = return $ map (\x -> (drop x (take (x+l) xs), x+1)) [0..(length xs)-l]

///////////////////////////////////////////////////////////////////
/// EXTRACTION FUNCTIONS
///////////////////////////////////////////////////////////////////

Extract an agent attribute from an array of strings

> extrAtt :: Ptr (CInt) -> IO Att
> extrAtt p = do 
>              arr  <- peekArray 2 p
>              let name  = fromIntegral(head arr)
>                  value = fromIntegral(head (tail arr)) in
>                return (name, value)

Extract an agent state from an array of arrays of strings

> extrAState :: Int -> Ptr (Ptr (CInt)) -> IO AState
> extrAState n p = do
>                  arr <- peekArray n p
>                  mapM extrAtt arr

Extract a group state from an array of arrays of arrays of strings

> extrGState :: Int -> Int -> Ptr (Ptr (Ptr (CInt))) -> IO GState
> extrGState nAgents nAtts p = do
>                              arr <- peekArray nAgents p          -- convert pointer to list of pointers
>                              l <- mapM (extrAState nAtts) arr -- call 'extrAState' for each pointer in list; 2 attributes per agent; TODO: make parameter
>                              return $ V.fromList l               -- convert list to Data.Vector

Extract a group life history from an array of arrays of arrays of arrays of strings

> extrGLH :: Ptr (Ptr (Ptr (Ptr (CInt)))) -> ReaderT CheckEnv IO GLH
> extrGLH p = do
>             env <- ask
>             let numAgents = rNumAgents env
>             let numAtts = rNumAtts env
>             let ticks = rTicks env
>             let numTicks = length ticks
>             arr <- lift $ peekArray numTicks p
>             glh <- lift $ mapM (extrGState numAgents numAtts) arr
>             return glh -- $ take (length glh) $!! glh -- use deepseq to force evaluation of glh

///////////////////////////////////////////////////////////////////
/// CHECKING FUNCTIONS
///////////////////////////////////////////////////////////////////

Helper function to create an environment for the Reader monad
param 1: configuration id
param 2: number of agents
param 3: number of agent attributes
param 4: number of ticks

> createEnv :: Int -> Int -> Int -> Int -> IO CheckEnv
> createEnv i numAgents numAtts numTicks = 
>    do
>    obl <- fFormula i numAgents 
>    return $ CheckEnv numAgents [0..(numTicks-1)] numAtts obl 0 i

> hoistState :: Monad m => State s a -> StateT s m a
> hoistState = StateT . (return .) . runState

Checks a GLTL formula on an executable simulation by using the predefined list of (not yet evaluated) GStates from the C++ library

> checkGLH' :: [IO GState] -> CheckEnv -> StateT CheckState IO (Maybe Bool)
> checkGLH' gs e = 
>          {-# SCC "checkGLH'" #-} 
>          let n = rNumAgents e 
>              a = rNumAtts e
>              o = rObligation e 
>              t = Main.rTick e in
>            case gs of 
>              []     -> return $ Just False -- TODO: used to be Nothing before. Is false ok??
>              [x]    -> do gstate <- lift x -- perform simulation step
>                           res <- hoistState $ checkGState o (GEnv gstate True [] Nothing t)
>                           case res of
>                             (True, GObl GTrue [])          -> return $ Just True -- formula is satisfied
>                             (True, GObl (GForall GTrue) _) -> return $ Just True -- formula is satisfied (TODO: is that OK??)
>                             (True, GGObl [])               -> return $ Just True
>                             (False, _)                     -> return $ Just False -- formula is refuted
>                             otherwise                      -> return Nothing -- formula is neither satisfied nor refuted
>              (x:xs) -> do gstate <- lift x
>                           res <- hoistState $ checkGState o (GEnv gstate False [] Nothing t)
>                           case res of 
>                             (False, _)                     -> return $ Just False -- formula is refuted
>                             (True, GObl GTrue [])          -> return $ Just True  -- no obligations for next state, check original formula
>                             (True, GObl (GForall GTrue) _) -> return $ Just True -- no obligations for next state, check original formula (TODO: is that OK??)
>                             (True, GGObl [])               -> return $ Just True -- no obligations for next state, check original formula
>                             (True, oNew)                   -> checkGLH' xs $ e { rObligation=oNew, Main.rTick=t+1 } -- only obligation needs to be checked; (TODO: is that correct??)

> checkGLH :: [IO GState] -> CheckEnv -> StateT CheckState IO (Maybe Bool)
> checkGLH gs e = {-# SCC "checkGLH" #-}
>                 do gen <- lift newStdGen             -- it's important to create a new RNG everytime the function is called in order to keep replications independent
>                    state <- get
>                    put state { rRandGen=gen }
>                    checkGLH' gs e

Calls the step function in the C++ library for the given tick

> stepABS :: Int -> Int -> Int -> Int -> IO GState
> stepABS id tick numAgents numAtts = do gs <- c_step (fromIntegral id) (fromIntegral tick)
>                                        extrGState numAgents numAtts gs

Incremental online checking function; runs the simulation and verifies the property on each suffix of the resulting GLH
param 1: replication number
param 2: fragment size

> estimate' :: Int -> Int -> ReaderT CheckEnv IO (Maybe Double)
> estimate' repId fs = do lift $ printf "Rep. %d; Checking mode: incremental online; " repId
>                         env <- ask
>                         let ticks = rTicks env
>                         let numTicks = length ticks
>                         let numAgents = rNumAgents env
>                         let numAtts = rNumAtts env
>                         let obl = rObligation env
>                         let confId = rConfId env
>                         rand <- lift newStdGen
>                         lift $ c_preRun (fromIntegral confId)
>                         let glh = [ stepABS confId t numAgents numAtts | t <- ticks ] -- create a list of thunks containing GState requests (for later evaluation) 
>                         glhs <- lift $ getSublists glh $ fromIntegral fs
>                         res' <- lift $ mapM (\glh -> runStateT (checkGLH (fst glh) env { Main.rTick=snd glh }) CheckState { rRandGen=rand, rBindings=[] }) glhs
>                         let res = successRatio $ map fst res'
>                         when (res == Just 0.0) $ lift $ c_writeGLHToFile (fromIntegral repId)
>                         lift $ printf "result = %s;\n" (show res) 
>                         lift $ c_tearDownRun (fromIntegral confId)
>                         return res

Estimates the probability of a formula on path fragments of the runs
param 1: configuration id
param 2: number of replications

> estimate :: Int -> Int -> ReaderT CheckEnv IO (Maybe Double)
> estimate fs nr = do env <- ask 
>                     let confId = rConfId env
>                     lift $ c_preConf (fromIntegral confId)                 -- setup configuration
>                     lift $ printf "Checking %d replication(s) ...\n" nr
>                     l <- mapM (\i -> estimate' i fs) [1..nr]          -- run check function once for each replication
>                     if (elem Nothing l) then                            -- if at least one subresult is Nothing, ...
>                       do
>                         lift $ c_tearDownConf (fromIntegral confId)
>                         return Nothing                                -- then the overall result is also Nothing
>                     else 
>                       let s = sum <$> sequence l in                   -- calculate sum of result list
>                       do   
>                         let res = (/) <$> s <*> Just (fromIntegral $ length l) -- calculate average of result list by dividing sum by number of elements in list
>                         lift $ output res "Configuration result"
>                         lift $ c_tearDownConf (fromIntegral confId)
>                         return res 


///////////////////////////////////////////////////////////////////
/// EVALUATION FUNCTIONS
///////////////////////////////////////////////////////////////////

Evaluate a configuration

> evalConf :: Conf -> IO (Maybe Double)
> evalConf p = do
>              let i   = pConfiguration p
>                  fs  = pFragmentSize p
>                  nag = pNumAgents p
>                  nat = pNumAtts p
>                  nt  = pNumTicks p 
>                  nr  = pNumReps p  in
>                    do
>                    printf "\n*** CHECKING CONFIGURATION %d ***\n" i
>                    env <- createEnv i nag nat nt
>                    do runReaderT (estimate fs nr) env 

Evaluate an experiment

> evalProp :: Prop -> IO (Maybe Double)
> evalProp (Atom c)     = do
>                        r <- evalConf c
>                        return r
> evalProp (Add c1 c2)  = do
>                        r1 <- evalProp c1
>                        r2 <- evalProp c2
>                        return $ (+) <$> r1 <*> r2 -- add up evaluation results
> evalProp (Sub c1 c2)  = do
>                        r1 <- evalProp c1
>                        r2 <- evalProp c2
>                        return $ (-) <$> r1 <*> r2 -- subtract evaluation results
> evalProp (Mult c1 c2) = do
>                        r1 <- evalProp c1
>                        r2 <- evalProp c2
>                        return $ (*) <$> r1 <*> r2 -- multiply evaluation results
> evalProp (Div c1 c2)  = do
>                        r1 <- evalProp c1
>                        r2 <- evalProp c2
>                        return $ (/) <$> r1 <*> r2 -- divide evaluation results

///////////////////////////////////////////////////////////////////
/// CONFIG PARAMS
///////////////////////////////////////////////////////////////////

Load the Property instance from the C++ library and return it as a string

> fProperty :: IO Prop
> fProperty = do
>             ptr <- c_getProperty
>             s <- peekCString ptr
>             return $ read s

Loading the formula from the C++ library and wrapping it into a GObligation

> fFormula :: Int -> Int -> IO GObligation
> fFormula i na = do
>                 ptr <- c_getFormula (fromIntegral i)
>                 s <- peekCString ptr
>                 let f = read s
>                 let fPNF = gltlToPNF f
>                 printf "Formula: %s\n" $ show f
>                 printf "Formula (PNF): %s\n" $ show fPNF
>                 return $ GObl fPNF [0..(na-1)] 

///////////////////////////////////////////////////////////////////
/// MAIN FUNCTION
///////////////////////////////////////////////////////////////////

Formatted output of Maybe Double 

> output :: Maybe Double -> String -> IO ()
> output Nothing  s = printf "%s: Nothing\n" s
> output (Just d) s = printf "%s: %.3f%%\n" s (d*100)

> main = do
>        prop <- fProperty                               -- read property from C++ file (in String representation)
>        res <- evalProp prop                             -- evaluate property
>        output res "RESULT"                             -- output result
>        return ()
