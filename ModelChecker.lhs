%/*
% * Copyright (c) 2014 Benjamin C. Herd.
% *
% * This file is part of MC2MABS.
% *
% * MC2MABS is free software: you can redistribute it and/or modify
% * it under the terms of the GNU General Public License as published by
% * the Free Software Foundation, either version 3 of the License, or
% * (at your option) any later version.
%
% * MC2MABS is distributed in the hope that it will be useful,
% * but WITHOUT ANY WARRANTY; without even the implied warranty of
% * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% * GNU General Public License for more details.
%
% * You should have received a copy of the GNU General Public License
% * along with MC2MABS. If not, see <http://www.gnu.org/licenses/>.
% */

\section{ModelChecker.lhs}

This file defines the actual monitoring algorithms. 

> module ModelChecker where

> import Foreign
> import Foreign.C
> import Foreign.C.Types

> import SimLTL
> import Data.Monoid
> import Data.List
> import Data.List.Utils
> import Control.Monad.Reader
> import Control.Monad.State
> import Control.Applicative
> import Debug.Trace
> import System.Random
> import qualified Data.Vector as V

\subsection{Foreign Functional Interface (FFI)}

> foreign import ccall "abs.h gValue" 
>   c_gValue :: CInt -> CInt -> CInt

> foreign import ccall "abs.h aValue" 
>   c_aValue :: CInt -> CInt -> CInt -> CInt

> foreign import ccall "abs.h gPredicate"
>   c_gPredicate :: CInt -> CInt -> Bool

> foreign import ccall "abs.h aPredicate"
>   c_aPredicate :: CInt -> CInt -> CInt -> Bool

\subsection{Definitions \& helper functions}

Results consist of a Boolean value (truth of the formula in the current state) and an obligation for the next state

> type AResult = (Bool, AObligation)
> type GResult = (Bool, GObligation)

Binary operations for the AResult type:

> aAnd :: AResult -> AResult -> AResult
> aAnd (True, f1) (True, f2)  = (True, simplifyA $ AAnd f1 f2) 
> aAnd _ _                    = (False, ATrue)

> aOr :: AResult -> AResult -> AResult
> aOr (False, f1) (False, f2)  = (False, ATrue)
> aOr (False, f1) (True, f2)   = (True, f2)
> aOr (True, f1) (False, f2)   = (True, f1)
> aOr (True, f1) (True, f2)    = (True, simplifyA $ AOr f1 f2)

> gAnd :: GResult -> GResult -> GResult
> gAnd (True, o1) (True, o2)  = (True, gOblAnd o1 o2) 
> gAnd _ _                    = (False, GObl GTrue [])

> gOr :: GResult -> GResult -> GResult
> gOr (False, o1) (False, o2)  = (False, GObl GTrue [])
> gOr (False, o1) (True, o2)   = (True, o2)
> gOr (True, o1) (False, o2)   = (True, o1)
> gOr (True, o1) (True, o2)    = (True, gOblOr o1 o2) -- why is that different from aOr above??

Extracts a sublist from a given list using the list of indices provided:

> sublist :: V.Vector a -> [Int] -> V.Vector a
> sublist xs is = V.fromList [ xs V.!i | i <- is ]

Extract a list of all suffixes from a list, i.e. suffixes [1,2,3] = [[1,2,3],[2,3],[3]]

> suffixes :: [a] -> [[a]]
> suffixes []   = [[]]
> suffixes [x]  = [[x]]
> suffixes xs   = xs : (suffixes $ tail xs)  

Do the same as suffixes but for Data.Vector

> suffixesV :: V.Vector a -> [V.Vector a]
> suffixesV v =  case V.length v of
>                  0 -> [V.empty]
>                  1 -> [V.singleton $ V.head v] 
>                  otherwise -> v : (suffixesV $ V.tail v)

\subsection{Semantic model}

\subsubsection{Agent level}

> type Name    = Int -- agent attribute name
> type Value   = Int -- agent attribute value
> type Att     = (Name, Value) -- agent attribute 
> type AState  = [Att] -- agent state (association list)
> type ATrace     = [AState] -- agent life history is a list of agent states

\subsubsection{Group level}

> type GState = V.Vector AState -- group state is a list of agent states
> type GTrace = [GState] -- group life history is a list of group states

\subsection{Model checking}

\subsubsection{Agent level}

The environment type for the agent level check:
- aState: current state of the ATrace that the formula is to be checked upon
- aIsLast: flag denoting whether the state is the final one
- aFormula: ALTL formula to be checked

> data AEnv =  AEnv { 
>                aAgentIdx :: Int, 
>                aState :: AState, 
>                aIsLast :: Bool, 
>                aTick :: Int
>              }

Checking an ALTL formula on an agent state:
Careful: ALTL input formula must be in PNF!!

> checkA :: ALTL -> AEnv -> State CheckState AResult
> checkA f e = 
>   let  id = aAgentIdx e
>        s = aState e
>        t = aTick e
>        l = aIsLast e in
>     case f of
>       ATrue             -> return (True, ATrue) -- True is always true
>       AFalse            -> return (False, ATrue) -- False is always false
>       ALast             -> return (l, ATrue)
>       (AAtom a)         -> let res = c_aPredicate (fromIntegral a) (fromIntegral id) (fromIntegral t) in
>                              return (res, ATrue) -- for the time being we assume that atomic predicates are always satisfied
>       (ANot f)          -> do  chk <- checkA f e
>                                let res = not $ fst $ chk
>                                case f of 
>                                  ATrue      -> return (res, ATrue)
>                                  AFalse     -> return (res, ATrue)
>                                  ALast      -> return (res, ATrue)
>                                  AAtom _    -> return (res, ATrue)
>                                  otherwise  -> error ("Non-atomic formula in negation: " ++ show f) 
>       (AAnd f1 f2)      -> do  r1 <- checkA f1 e
>                                r2 <- checkA f2 e
>                                return $ r1 `aAnd` r2 
>       (AOr f1 f2)       -> do  r1 <- checkA f1 e
>                                r2 <- checkA f2 e
>                                return $ r1 `aOr` r2
>       (ANext f)         -> if l then 
>                              return (False, ATrue)
>                            else 
>                              return (True, f)
>       (ASNext f)        -> return (True, f)
>       (AUntil f1 f2)    -> do  r1 <- checkA f1 e
>                                r2 <- checkA f2 e
>                                r3 <- return (not l, ATrue)
>                                if l then return r2
>                                else 
>                                  return $ r2 `aOr` (r1 `aAnd` (True, AUntil f1 f2))
>       (ARelease f1 f2)  -> do  r1 <- checkA f1 e
>                                r2 <- checkA f2 e
>                                r3 <- return (l, ATrue)
>                                if l then return r2
>                                else return $ r2 `aAnd` (r1 `aOr` (True, ARelease f1 f2))
>       (AFinally f)      -> do  r <- checkA f e
>                                if l then return r
>                                else return $ r `aOr` (True, AFinally f)
>       (AGlobally f)     -> do  r <- checkA f e
>                                if l then return r
>                                else return $ r `aAnd` (True, AGlobally f)
>       (AVal v)          -> do  evaluateA t id s v
>                                return (True, ATrue) 
>       (AEq v1 v2)       -> do  r1 <- evaluateA t id s v1
>                                r2 <- evaluateA t id s v2 
>                                return $ (r1 == r2, ATrue)
>       (ANEq v1 v2)      -> do  r1 <- evaluateA t id s v1
>                                r2 <- evaluateA t id s v2 
>                                return $ (r1 /= r2, ATrue)
>       (AAEq d v1 v2)    -> do  r1 <- (evaluateA t id s v1) 
>                                r2 <- (evaluateA t id s v2) 
>                                let diff = fromIntegral <$> ((-) <$> r1 <*> r2)    -- determine difference between two Maybe Ints and turn into Maybe Num (to be compatible with Double threshold)
>                                let res = (<=) <$> diff <*> Just d                 -- check whether difference is <= d
>                                case res of 
>                                  Nothing -> error "Approximately equal resulted in Nothing"
>                                  Just b  -> return (b, ATrue)
>       (ANAEq d v1 v2)   -> do  r1 <- (evaluateA t id s v1) 
>                                r2 <- (evaluateA t id s v2) 
>                                let diff = fromIntegral <$> ((-) <$> r1 <*> r2)   -- determine difference between two Maybe Ints and turn into Maybe Num (to be compatible with Double threshold)
>                                let res = (>) <$> diff <*> Just d                 -- check whether difference is > d
>                                case res of 
>                                  Nothing -> error "Approximately equal resulted in Nothing"
>                                  Just b  -> return (b, ATrue)
>       (ALt v1 v2)       -> do  r1 <- evaluateA t id s v1
>                                r2 <- evaluateA t id s v2 
>                                return $ (r1 < r2, ATrue)
>       (ALEq v1 v2)      -> do  r1 <- evaluateA t id s v1
>                                r2 <- evaluateA t id s v2 
>                                return $ (r1 <= r2, ATrue)
>       (AGt v1 v2)       -> do  r1 <- evaluateA t id s v1
>                                r2 <- evaluateA t id s v2 
>                                return $ (r1 > r2, ATrue)
>       (AGEq v1 v2)      -> do  r1 <- evaluateA t id s v1
>                                r2 <- evaluateA t id s v2 
>                                return $ (r1 >= r2, ATrue)
>       otherwise         -> error $ ("\nNon-atomic agent formula: " ++ show f)
 
Evaluate an expression on a state; \\
Requested attribute may not exist, therefore return type is 'Maybe Value'\\
param 1: current tick\\
param 2: agent id\\
param 3: agent state\\
param 4: AVal instance\\

> evaluateA :: Int -> Int -> AState -> AVal -> State CheckState (Maybe Value)
> evaluateA _ _ _ (ANumVal v)      = return $ Just v
> evaluateA _ _ s (AAttribute v)   = return $ lookup v s
> evaluateA t id _ (ANumFunc f)    = return $ Just $ fromIntegral $ c_aValue (fromIntegral f) (fromIntegral id) (fromIntegral t)
> evaluateA t id s (AAsg k v)      = do  v' <- evaluateA t id s v
>                                        state <- appendBinding (k, v')
>                                        return v'
> evaluateA _ _ _ (AVar k)         = do  state <- get
>                                        let res = lookup k $ rBindings state
>                                        return res
> evaluateA t id s (APlus v1 v2)   = do  r1 <- evaluateA t id s v1
>                                        r2 <- evaluateA t id s v2
>                                        return $ (+) <$> r1 <*> r2
> evaluateA t id s (AMinus v1 v2)  = do  r1 <- evaluateA t id s v1
>                                        r2 <- evaluateA t id s v2
>                                        return $ (-) <$> r1 <*> r2
> evaluateA t id s (ATimes v1 v2)  = do  r1 <- evaluateA t id s v1
>                                        r2 <- evaluateA t id s v2
>                                        return $ (*) <$> r1 <*> r2

\subsubsection{Group level}

The environment type for the group level check:\\
- rState: current state of the GTrace that the formula is to be checked upon\\
- rIsLast: flag denoting whether the state is the final one\\
- rAgents: current selection, i.e. relevant agents\\
- rSubA: substitution of agent index\\
- rRandGen: random number generator\\
- rTick: current tick\\

> data GEnv =  GEnv { 
>                rState :: GState, 
>                rIsLast :: Bool, 
>                rAgents :: [Int],
>                rSubA :: Maybe Int,
>                rTick :: Int
>              }

> data CheckState =  CheckState {
>                      rRandGen :: StdGen,
>                      rBindings :: [(String, Value)]
>                    }

Checks an LTL formula on an individual global state

> checkGState' :: GLTL -> GEnv -> State CheckState GResult
> checkGState' ff e = do 
>    let s =  rState e 
>        l =  rIsLast e
>        a =  rAgents e 
>        i =  rSubA e
>        t =  rTick e in
>      case ff of
>        GTrue             -> return $ (True, GObl GTrue []) -- True is always true
>        GFalse            -> return $ (False, GObl GTrue []) -- False is always false
>        GLast             -> return $ (l, GObl GTrue [])
>        (GAtom a)         -> let res = c_gPredicate (fromIntegral a) (fromIntegral t) in
>                               return $ (res, GObl GTrue []) -- for the time being we assume that atomic predicates are always true
>        (GNot f)          -> do  chk <- checkGState' f e 
>                                 let res = not $ fst chk in -- this should work because all negations should have been pushed inward scase f of 
>                                   case f of 
>                                     GTrue     ->  return $ (res, GObl GTrue [])
>                                     GFalse    ->  return $ (res, GObl GTrue [])
>                                     GLast     ->  return $ (res, GObl GTrue [])
>                                     GAtom _   ->  return $ (res, GObl GTrue [])
>                                     otherwise ->  error ("Non-atomic formula in negation: " ++ show f) 
>        (GAnd f1 f2)     -> do  r1 <- checkGState' f1 e
>                                r2 <- checkGState' f2 e
>                                return $ r1 `gAnd` r2
>        (GOr f1 f2)      -> do  r1 <- checkGState' f1 e
>                                r2 <- checkGState' f2 e
>                                return $ r1 `gOr` r2
>        (GNext f)        -> return $ (True, GObl f [])
>        (GUntil f1 f2)   -> do  r1 <- checkGState' f1 e
>                                r2 <- checkGState' f2 e
>                                r3 <- return (not l, GObl GTrue [])
>                                if l then return r2 
>                                else return $ r2 `gOr` (r1 `gAnd` (True, GObl (GUntil f1 f2) a))
>        (GRelease f1 f2) -> do  r1 <- checkGState' f1 e
>                                r2 <- checkGState' f2 e
>                                r3 <- return (l, GObl GTrue [])
>                                if l then return r2
>                                else
>                                  return $ r2 `gAnd` (r1 `gOr` (True, GObl (GRelease f1 f2) a))
>        (GFinally f)     -> do  r <- checkGState' f e
>                                if l then return r
>                                else return $ r `gOr` (True, GObl (GFinally f) a)
>        (GGlobally f)    -> do  r <- checkGState' f e
>                                if l then  return r
>                                else return $ r `gAnd` (True, GObl (GGlobally f) a)
>        (GALTL f1)       -> do  state <- get
>                                let (idx, g') = pick (rRandGen state) i a               -- pick agent state
>                                put state { rRandGen=g' }                    -- update RNG in state
>                                let f1' = altlToPNF f1                   -- translate inner agent formula into PNF
>                                (b1,b2) <- checkA f1' $ AEnv idx (s V.! idx) l t
>                                if (b1 && (b2 == ATrue)) then        -- if both immediate and future obligation for the agent has been satisfied ... 
>                                  return (True, GObl GTrue [])       -- ... then the GALTL formula is satisfied!
>                                else if b1 then                      -- Otherwise if only the immediate obligation for the agent has been satisfied ...
>                                  return (True, GObl (GALTL b2) [idx])    -- ... then the immediate obligation of the GALTL formula is true but future obligation is created.
>                                else return (False, GObl GTrue [])   -- Otherwise, the GALTL formula is refuted. 
>        (GSel f1 f2)     -> do  chk' <- V.mapM (\(id, x) -> checkA f1 $ AEnv id x l t) $ V.zip (V.fromList a) (sublist s a) -- check property on all relevant agent states
>                                let chk = V.toList $ V.zip (V.fromList a) chk'            -- convert back in list
>                                let res' = filter (\(_,(b, _)) -> b) chk -- find all agents for which the check was successful (i.e. returned True) 
>                                res <- checkGState' f2 e { rAgents=[fst r | r <- res'] }
>                                return res
>        (GForall f)      -> do  chk <- mapM (\id -> checkGState' f e { rSubA=Just id}) a     -- check group property once for each agent ...  
>                                let  res = zip a chk                         -- ... and enumerate results with agent ids 
>                                     r1 = {-# SCC "forall_2" #-} all (\(_,(b,_)) -> b) res -- check whether all agents returned true using 'all'
>                                     r2 = {-# SCC "forall_3" #-} GGObl (filter (\x -> fst x /= (GObl GTrue [])) [(o,i) | (i,(_,o)) <- res]) -- filter all results for which the obligation is non-empty => create set of individual obligations; due to lazyness, this is only created if r1==True (see next line)
>                                {-# SCC "forall" #-} if (r1 && (r2 == (GGObl []))) then     -- if no obligations have been created for the individual agents ... 
>                                                       return (True, GObl GTrue [])     -- ... then we're done.
>                                                     else if r1 then                        -- Otherwise if the immediate obligations for all agents have been satisfied ... 
>                                                       return (True, r2)            -- ... then the immediate obligation of0 the GForall formula is satisfied but a future one is created.
>                                                     else return (False, GObl GTrue [])  -- Otherwise, the GForall formula is refuted. 
>        (GExist i f)     -> do  chk' <- mapM (\id -> checkGState' f e { rSubA=Just id }) a      -- check property on all relevant agent states ...
>                                let  chk = zip a chk'                          -- ... and enumerate results with agent ids
>                                     res = filter (\(_,(b,_)) -> b) chk        -- find all agents for which the check was successful (i.e. returned True)  
>                                     io = length res >= i                      -- check whether the number of successful agents is greater than or equal to i
>                                     fo = {-# SCC "exist_2" #-} GGObl (filter (\x -> fst x /= (GObl GTrue [])) [(o,i) | (i,(_,o)) <- res]) -- filter all results for which the obligation is non-empty => create set of individual obligations; due to lazyness, this is only created if r1==True (see next line)
>                                if (io && (fo == GGObl [])) then            -- if no obligations have been created for the individual agents ... 
>                                  return (True, GObl GTrue [])              -- ... then we're done.
>                                else if io then                             -- Otherwise if the immediate obligations for all agents have been satisfied ...
>                                  {-# SCC "exist_3" #-} return $ (True, fo) -- ... then the immediate obligation of the GExist formula is satisfied but a future one is created.
>                                else return $ (False, GObl GTrue [])        -- Otherwise, the GExist formula is refuted.
>        (GVal v)         -> do  let sl = sublist s a
>                                evaluateG t sl v
>                                return (True, GObl GTrue []) 
>        (GEq v1 v2)      -> do  let sl = sublist s a
>                                r1 <- (evaluateG t sl v1) 
>                                r2 <- (evaluateG t sl v2) 
>                                return (r1 == r2, GObl GTrue [])
>        (GNEq v1 v2)     -> do  let sl = sublist s a
>                                r1 <- (evaluateG t sl v1) 
>                                r2 <- (evaluateG t sl v2) 
>                                return (r1 /= r2, GObl GTrue [])
>        (GAEq d v1 v2)   -> do  let sl = sublist s a
>                                r1 <- (evaluateG t sl v1) 
>                                r2 <- (evaluateG t sl v2) 
>                                let diff = fromIntegral <$> ((-) <$> r1 <*> r2)    -- determine difference between two Maybe Ints and turn into Maybe Num (to be compatible with Double threshold)
>                                let res = (<=) <$> diff <*> Just d                 -- check whether difference is <= d
>                                case res of 
>                                  Nothing -> error "Approximately equal resulted in Nothing"
>                                  Just b  -> return (b, GObl GTrue [])
>        (GNAEq d v1 v2)  -> do  let sl = sublist s a
>                                r1 <- (evaluateG t sl v1) 
>                                r2 <- (evaluateG t sl v2) 
>                                let diff = fromIntegral <$> ((-) <$> r1 <*> r2)    -- determine difference between two Maybe Ints and turn into Maybe Num (to be compatible with Double threshold)
>                                let res = (>) <$> diff <*> Just d                 -- check whether difference is > d
>                                case res of 
>                                  Nothing -> error "Approximately equal resulted in Nothing"
>                                  Just b  -> return (b, GObl GTrue [])
>        (GLt v1 v2)      -> do  let sl = sublist s a
>                                r1 <- (evaluateG t sl v1) 
>                                r2 <- (evaluateG t sl v2) 
>                                return (r1 < r2, GObl GTrue [])
>        (GLEq v1 v2)     -> do  let sl = sublist s a
>                                r1 <- (evaluateG t sl v1) 
>                                r2 <- (evaluateG t sl v2) 
>                                return (r1 <= r2, GObl GTrue [])
>        (GGt v1 v2)      -> do  let sl = sublist s a
>                                r1 <- (evaluateG t sl v1) 
>                                r2 <- (evaluateG t sl v2) 
>                                return (r1 > r2, GObl GTrue [])
>        (GGEq v1 v2)     -> do  let sl = sublist s a
>                                r1 <- (evaluateG t sl v1) 
>                                r2 <- (evaluateG t sl v2) 
>                                return (r1 >= r2, GObl GTrue [])
>        otherwise        -> error $ ("\nNon-atomic group formula: " ++ show ff)

Checks an obligation on an individual global state
Signature: obligation -> global state -> last state? -> id of current agent -> (Boolean result, obligation)

> checkGState :: GObligation -> GEnv -> State CheckState GResult
> checkGState o e = {-# SCC "checkGState" #-} do
>   let  s = rState e
>        l = rIsLast e
>        i = rSubA e
>        t = rTick e in
>     case o of 
>       (GObl f is)   -> do  res <- checkGState' f e { rAgents=is }
>                            return res
>       (GGObl xs)    -> do  res <- checkGGObl xs e
>                            return res
>       (Disj f1 f2)  -> do  (b1, o1) <- checkGState f1 e
>                            (b2, o2) <- checkGState f2 e
>                            return (b1 || b2, gOblAnd o1 o2) -- TODO: should that really be gOblAnd? 
>       (Conj f1 f2)  -> do  (b1, o1) <- checkGState f1 e
>                            (b2, o2) <- checkGState f2 e
>                            return (b1 && b2, gOblAnd o1 o2) 

Pick agent from the set of agent states
If no particular agent index is currently in scope, return random state

> pick :: RandomGen g => g -> Maybe Int -> [Int] -> (Int, g)
> pick g Nothing l   =  let rand = randomR (0, (length l)-1) g in  -- no particular index exists => determine random number
>                         let  idx' = fst rand                      -- random list index
>                              gen' = snd rand in                   -- new generator
>                           (l!!idx', gen')                        -- return element at index and new generator
> pick g (Just i) l  = (i, g) -- index exists => return it

Check a set of group obligations

> checkGGObl :: [(GObligation,Int)] -> GEnv -> State CheckState GResult
> checkGGObl xs e = 
>   let  s = rState e
>        l = rIsLast e 
>        t = rTick e in
>     do  chk <- mapM (\(obl, idx) -> checkGState obl e { rAgents=[], rSubA=Just idx }) xs -- check obligations for all agents denoted by given list indices
>         let  res = zip [snd s | s <- xs] chk
>              io = all (\(_,(i,_)) -> i) res -- check whether all agents returned true (using foldr) => Boolean result (note: changing this from foldl to foldr seems to have speeded up the program significantly!!)
>              fo = GGObl $ filter (\x -> fst x /= (GObl GTrue [])) [(o,i) | (i,(_,o)) <- res] -- create individual obligations
>         if (io && (fo == GGObl [])) then
>           return (True, GObl GTrue [])
>         else if io then
>           return (True, fo) 
>         else return (False, GObl GTrue [])

Creates a conjunction of two obligations:

> gOblAnd :: GObligation -> GObligation -> GObligation
> gOblAnd (GObl GTrue _) (GObl GTrue _)  = GObl GTrue [] -- if both obligations are true -> combine to a single true obligation
> gOblAnd (GObl GTrue []) o              = o
> gOblAnd o (GObl GTrue [])              = o
> gOblAnd (GObl f1 is1) (GObl f2 is2)    = if is1==is2 -- if both obligations concern the same group of agents ...
>                                            then GObl (simplifyG $ GAnd f1 f2) is1 -- then combine them into a single obligation ...
>                                            else Conj (GObl f1 is1) (GObl f2 is2) -- else create a conjunction of obligations
> gOblAnd (GGObl []) (GGObl [])          = GObl GTrue []
> gOblAnd (GGObl []) o                   = o
> gOblAnd o (GGObl [])                   = o
> gOblAnd o1 o2                          = Conj o1 o2

Creates a disjunction of two obligations:

> gOblOr :: GObligation -> GObligation -> GObligation
> gOblOr (GObl GTrue _) _             = GObl GTrue [] -- if one obligation is true -> combine to a single true obligation
> gOblOr _ (GObl GTrue _)             = GObl GTrue [] -- if one obligation is true -> combine to a single true obligation
> gOblOr (GObl f1 is1) (GObl f2 is2)  = if is1==is2 then -- if both obligations concern the same group of agents ...
>                                         GObl (simplifyG $ GOr f1 f2) is1 -- then combine them into a single obligation ...
>                                         else Disj (GObl f1 is1) (GObl f2 is2) -- else create a disjunction of obligations
> gOblOr (GGObl []) _                 = GObl GTrue []
> gOblOr _ (GGObl [])                 = GObl GTrue []

Evaluates a value on a global state:
param 1: current tick
param 2: group state
param 3: GVal instance

Appends a variable binding to the binding table in the current state

> appendBinding :: (String, Maybe Value) -> State CheckState ()
> appendBinding (k, Nothing) =  return ()
> appendBinding (k, Just v)  =  do state <- get
>                                  let bindings = rBindings state
>                                  let bindings' = addToAL bindings k v -- let bindings' = (k,v) : bindings
>                                  put state { rBindings=bindings' }

> evaluateG :: Int -> GState -> GVal -> State CheckState (Maybe Value)
> evaluateG _ _ (GNumVal v)       = return $ Just v
> evaluateG t _ (GNumFunc f)      = return $ Just $ fromIntegral $ c_gValue (fromIntegral f) (fromIntegral t)
> evaluateG t s (GAsg k v)        = do  v' <- evaluateG t s v
>                                       state <- appendBinding (k, v')
>                                       return v'
> evaluateG _ _ (GVar k)         = do  state <- get
>                                      return $ lookup k $ rBindings state
> evaluateG t s (GPlus v1 v2)    = do  r1 <- evaluateG t s v1
>                                      r2 <- evaluateG t s v2
>                                      return $ (+) <$> r1 <*> r2
> evaluateG t s (GMinus v1 v2)   = do  r1 <- evaluateG t s v1
>                                      r2 <- evaluateG t s v2
>                                      return $ (-) <$> r1 <*> r2
> evaluateG t s (GTimes v1 v2)   = do  r1 <- evaluateG t s v1
>                                      r2 <- evaluateG t s v2
>                                      return $ (*) <$> r1 <*> r2
  
Calculates the ratio of successes (i.e. 'Just True') in a list of 'Maybe Bool'

> successRatio :: [Maybe Bool] -> Maybe Double
> successRatio xs = do  seq <- sequence xs
>                       let succ = fromIntegral $ length $ filter id seq
>                       return $ succ / (fromIntegral $ length xs)

\subsection{Pretty printing}

For agent traces:

> ppATrace :: ATrace -> String
> ppATrace []      = ""
> ppATrace (x:xs)  = show x ++ " -> " ++ ppATrace xs

For group traces:

> ppGTrace :: GTrace -> String
> ppGTrace []      = ""
> ppGTrace (x:xs)  = show x ++ " -> " ++ ppGTrace xs

****************************************************



