> module SimLTL where
> import Debug.Trace

**** 1.1 Agent level **************************************************

An agent value: either an integer, numeric function, the value of a stored variable or an assignment:

> data AVal = ANumVal Int 
>    | AAttribute Int
>    | ANumFunc Int
>    | AAsg String AVal
>    | AVar String
>    | APlus AVal AVal
>    | AMinus AVal AVal
>    | ATimes AVal AVal
>  deriving (Read, Eq)

> instance Show AVal where
>   show v = ppAVal v

An agent formula: 

> data ALTL = 
>    ATrue | AFalse | ALast | AAtom Int
>    | ANot ALTL | AAnd ALTL ALTL | AOr ALTL ALTL | AImpl ALTL ALTL | AEquiv ALTL ALTL
>    | AFinally ALTL | ANext ALTL | ASNext ALTL | AGlobally ALTL
>    | AUntil ALTL ALTL | AWUntil ALTL ALTL | ARelease ALTL ALTL
>    | ATransInto ALTL ALTL
>    | AVal AVal | AEq AVal AVal | ANEq AVal AVal | AAEq Double AVal AVal | ANAEq Double AVal AVal | AGt AVal AVal | AGEq AVal AVal | ALt AVal AVal | ALEq AVal AVal   
>      deriving (Read, Eq)

> instance Show ALTL where
>   show v = ppALTL v

*** 1.2 Group level ***************************************************

A group value: either an integer, numeric function, the value of a stored variable or an assignment:

> data GVal = 
>    GNumVal Int 
>    | GNumFunc Int
>    | GAsg String GVal
>    | GVar String
>    | GPlus GVal GVal
>    | GMinus GVal GVal
>    | GTimes GVal GVal
>      deriving (Read, Eq)

> instance Show GVal where
>   show v = ppGVal v

> data GLTL = 
>    GTrue | GFalse | GLast | GAtom Int
>    | GNot GLTL | GAnd GLTL GLTL | GOr GLTL GLTL | GImpl GLTL GLTL | GEquiv GLTL GLTL
>    | GFinally GLTL | GNext GLTL | GSNext GLTL | GGlobally GLTL
>    | GUntil GLTL GLTL | GWUntil GLTL GLTL | GRelease GLTL GLTL
>    | GTransInto GLTL GLTL 
>    | GALTL ALTL | GSel ALTL GLTL | GForall GLTL | GForallA ALTL | GExist Int GLTL
>    | GVal GVal | GEq GVal GVal | GNEq GVal GVal | GAEq Double GVal GVal | GNAEq Double GVal GVal | GGt GVal GVal | GGEq GVal GVal | GLt GVal GVal | GLEq GVal GVal
>      deriving (Read, Eq)

> instance Show GLTL where
>   show v = ppGLTL v

A group obligation is either an obligation for the entire group, a disjunction of agent obligations
or a conjunction of agent obligations:

> type AObligation = ALTL

> data GObligation = 
>   GObl GLTL [Int] 
>   | GGObl [(GObligation, Int)]
>   | Disj GObligation GObligation 
>   | Conj GObligation GObligation
>     deriving (Eq, Show)


************************************************************************

**** TRANSFORMATIONS ***************************************************

Simplification (exploit equality laws)
TODO: - extend (http://www.tjhsst.edu/~rlatimer/ai/Logic/logicTable/node1.html)
      - make more elegant (terrible mess)

> simplifyA :: ALTL -> ALTL
> simplifyA f = case f of 
>                (AAnd ATrue f)             -> f -- ? 
>                (AAnd f ATrue)             -> f -- ?
>                (AAnd f1 (AOr f2 f3))      -> if f1==f2 then f1 else f -- absorption
>                (AAnd f1 (AAnd f2 f3))     -> if f1==f2 then simplifyA (AAnd f1 f3) else if f1==f3 then simplifyA (AAnd f1 f2) else f
>                (AAnd f1 f2)               -> if f1==f2 then f1 else f -- idempotence
>                (AOr ATrue f)              -> ATrue -- ? 
>                (AOr f ATrue)              -> ATrue -- ?
>                (AOr f1 (AAnd f2 f3))      -> if f1==f2 then f1 else f -- absorption 
>                (AOr f1 (AOr f2 f3))       -> if f1==f2 then simplifyA (AOr f1 f3) else if f1==f3 then simplifyA (AOr f1 f2) else f
>                (AOr f1 f2)                -> if f1==f2 then f1 else f -- idempotence
>                (AUntil f1 (AUntil f2 f3)) -> if f1==f2 then AUntil f1 f3 else f -- idempotence 
>                (AUntil (AUntil f1 f2) f3) -> if f2==f3 then AUntil f1 f3 else f -- idempotence 
>                otherwise                  -> f 

> simplifyG :: GLTL -> GLTL
> simplifyG f = case f of 
>                (GAnd f1 (GOr f2 f3))      -> if f1==f2 then f1 else f -- absorption
>                (GAnd f1 (GAnd f2 f3))     -> if f1==f2 then simplifyG (GAnd f1 f3) else if f1==f3 then simplifyG (GAnd f1 f2) else f
>                (GAnd f1 f2)               -> if f1==f2 then f1 else f -- idempotence
>                (GOr (GForall GTrue) f)    -> f
>                (GOr f1 (GAnd f2 f3))      -> if f1==f2 then f1 else f -- absorption 
>                (GOr f1 (GOr f2 f3))       -> if f1==f2 then simplifyG (GOr f1 f3) else if f1==f3 then simplifyG (GOr f1 f2) else f
>                (GOr f1 f2)                -> if f1==f2 then f1 else f -- idempotence
>                (GUntil f1 (GUntil f2 f3)) -> if f1==f2 then GUntil f1 f2 else f -- idempotence 
>                (GUntil (GUntil f1 f2) f3) -> if f2==f3 then GUntil f1 f2 else f -- idempotence 
>                (GExist _ GTrue)           -> GTrue -- TODO: correct??
>                otherwise                  -> f 

Transformation of ALTL into (release) positive normal form (PNF):

> altlToPNF :: ALTL -> ALTL
> altlToPNF (AAnd f1 f2)       = simplifyA $ AAnd (altlToPNF f1) (altlToPNF f2)
> altlToPNF (AOr f1 f2)        = simplifyA $ AOr (altlToPNF f1) (altlToPNF f2)
> altlToPNF (AWUntil f1 f2)    = simplifyA $ altlToPNF $ AOr (AUntil (altlToPNF f1) (altlToPNF f2)) $ AGlobally f1
> altlToPNF (AUntil f1 f2)     = simplifyA $ AUntil (altlToPNF f1) (altlToPNF f2)
> altlToPNF (ATransInto f1 f2) = simplifyA $ altlToPNF $ AWUntil (ANot f2) f1
> altlToPNF (AImpl f1 f2)      = simplifyA $ altlToPNF $ AOr (ANot f1) f2
> altlToPNF (AEquiv f1 f2)     = simplifyA $ altlToPNF $ AAnd (AImpl f1 f2) (AImpl f2 f1)
> altlToPNF (ARelease f1 f2)   = simplifyA $ ARelease (altlToPNF f1) (altlToPNF f2)
> altlToPNF (AGlobally f)      = simplifyA $ AGlobally (altlToPNF f)
> altlToPNF (AFinally f)       = simplifyA $ AFinally (altlToPNF f)
> altlToPNF (ANext f)          = ANext (altlToPNF f)
> altlToPNF (ASNext f)         = ASNext (altlToPNF f)
> altlToPNF (AEq v1 v2)        = AEq v1 v2
> altlToPNF (ANEq v1 v2)       = ANEq v1 v2
> altlToPNF (ALt v1 v2)        = ALt v1 v2
> altlToPNF (ALEq v1 v2)       = ALEq v1 v2
> altlToPNF (AGt v1 v2)        = AGt v1 v2
> altlToPNF (AGEq v1 v2)       = AGEq v1 v2
> altlToPNF (ANot f)           = case f of
>     ATrue             -> AFalse
>     AFalse            -> ATrue
>     AAtom s           -> ANot $ AAtom s -- remains unchanged
>     AAnd f1 f2        -> simplifyA $ altlToPNF $ AOr (ANot f1) (ANot f2)
>     AOr f1 f2         -> simplifyA $ altlToPNF $ AAnd (ANot f1) (ANot f2)
>     AUntil f1 f2      -> simplifyA $ altlToPNF $ ARelease (ANot f1) (ANot f2)
>     AWUntil f1 f2     -> simplifyA $ altlToPNF $ AOr (AUntil (altlToPNF f1) (altlToPNF f2)) $ AGlobally f1
>     ATransInto f1 f2  -> simplifyA $ altlToPNF $ ANot $ AWUntil (ANot f2) f1
>     AImpl f1 f2       -> simplifyA $ altlToPNF $ AAnd f1 (ANot f2)
>     ARelease f1 f2    -> simplifyA $ altlToPNF $ AUntil (ANot f1) (ANot f2)
>     AGlobally f       -> simplifyA $ altlToPNF $ AFinally $ ANot f
>     AFinally f        -> simplifyA $ altlToPNF $ AGlobally $ ANot f
>     ANext f           -> ASNext $ simplifyA $ altlToPNF $ ANot f
>     ASNext f          -> ANext $ simplifyA $ altlToPNF $ ANot f
>     AEq v1 v2         -> ANEq v1 v2
>     ANEq v1 v2        -> AEq v1 v2
>     ALt v1 v2         -> AGEq v1 v2
>     ALEq v1 v2        -> AGt v1 v2
>     AGt v1 v2         -> ALEq v1 v2
>     AGEq v1 v2        -> ALt v1 v2
>     ANot f            -> altlToPNF f -- double negation
>     ALast             -> ANot ALast -- remains unchanged
> altlToPNF f                  = f -- all other cases remain unchanged

Example:
let a = ANot (AGlobally (AOr (AUntil (AAtom "a") (AAtom "b") ) (ANext (AAtom "c")) ))

Conjunction of ALTL formulae:

> conjoin :: ALTL -> ALTL -> ALTL
> conjoin f1 f2 = simplifyA $ AAnd f1 f2

Transformation of GLTL into (release) positive normal form (PNF):

*******************************************************************************

> gltlToPNF :: GLTL -> GLTL
> gltlToPNF GTrue              = GTrue
> gltlToPNF GFalse             = GFalse
> gltlToPNF (GAtom s)          = GAtom s
> gltlToPNF (GALTL f)          = GALTL $ altlToPNF f
> gltlToPNF (GAnd f1 f2)       = simplifyG $ GAnd (gltlToPNF f1) (gltlToPNF f2)
> gltlToPNF (GOr f1 f2)        = simplifyG $ GOr (gltlToPNF f1) (gltlToPNF f2)
> gltlToPNF (GImpl f1 f2)      = simplifyG $ gltlToPNF $ GOr (GNot f1) f2
> gltlToPNF (GEquiv f1 f2)     = simplifyG $ gltlToPNF $ GAnd (GImpl f1 f2) (GImpl f2 f1)
> gltlToPNF (GRelease f1 f2)   = simplifyG $ GRelease (gltlToPNF f1) (gltlToPNF f2)
> gltlToPNF (GGlobally f)      = simplifyG $ GGlobally (gltlToPNF f)
> gltlToPNF (GFinally f)       = simplifyG $ GFinally (gltlToPNF f)
> gltlToPNF (GUntil f1 f2)     = simplifyG $ GUntil (gltlToPNF f1) (gltlToPNF f2)
> gltlToPNF (GWUntil f1 f2)    = simplifyG $ gltlToPNF $ GOr (GUntil (gltlToPNF f1) (gltlToPNF f2)) $ GGlobally f1
> gltlToPNF (GTransInto f1 f2) = simplifyG $ gltlToPNF $ GWUntil (GNot f2) f1
> gltlToPNF (GNext f)          = GNext $ gltlToPNF f
> gltlToPNF (GSNext f)         = GSNext $ gltlToPNF f
> gltlToPNF (GForall f)        = GForall $ gltlToPNF f
> gltlToPNF (GExist i f)       = GExist i $ gltlToPNF f
> gltlToPNF (GEq v1 v2)        = GEq v1 v2
> gltlToPNF (GNEq v1 v2)       = GNEq v1 v2
> gltlToPNF (GLt v1 v2)        = GLt v1 v2
> gltlToPNF (GGt v1 v2)        = GGt v1 v2
> gltlToPNF (GNot f)           = case f of
>     GTrue             -> GFalse
>     GFalse            -> GTrue
>     GAtom s           -> GNot $ GAtom s -- remains unchanged
>     GALTL f           -> GALTL $ altlToPNF $ ANot f
>     GAnd f1 f2        -> simplifyG $ gltlToPNF $ GOr (GNot f1) (GNot f2)
>     GOr f1 f2         -> simplifyG $ gltlToPNF $ GAnd (GNot f1) (GNot f2)
>     GUntil f1 f2      -> simplifyG $ gltlToPNF $ GRelease (GNot f1) (GNot f2)
>     GWUntil f1 f2     -> simplifyG $ gltlToPNF $ GOr (GUntil (gltlToPNF f1) (gltlToPNF f2)) $ GGlobally f1
>     GTransInto f1 f2  -> simplifyG $ gltlToPNF $ GNot $ GWUntil (GNot f2) f1
>     GImpl f1 f2       -> simplifyG $ gltlToPNF $ GAnd f1 (GNot f2)
>     GRelease f1 f2    -> simplifyG $ gltlToPNF $ GUntil (GNot f1) (GNot f2)
>     GGlobally f       -> simplifyG $ gltlToPNF $ GFinally $ GNot f
>     GFinally f        -> simplifyG $ gltlToPNF $ GGlobally $ GNot f
>     GNext f           -> GSNext $ simplifyG $ gltlToPNF $ GNot f
>     GSNext f          -> GNext $ simplifyG $ gltlToPNF $ GNot f
>     GSel f1 f2        -> GSel f1 $ simplifyG $ gltlToPNF $ GNot f2 -- TODO: is that correct??
>     GForall f         -> GExist 1 $ simplifyG $ gltlToPNF $ GNot f 
>     GExist i f        -> GNot $ GExist i $ simplifyG $ gltlToPNF f
>     GEq v1 v2         -> GNEq v1 v2
>     GGEq v1 v2        -> GLt v1 v2
>     GNEq v1 v2        -> GEq v1 v2
>     GLt v1 v2         -> GGt v1 v2
>     GGt v1 v2         -> GLt v1 v2
>     GNot f            -> gltlToPNF f -- double negation
>     GLast             -> GNot GLast
>     otherwise         -> error ("unhandled case: " ++ show otherwise)
> gltlToPNF f                  = f -- all other cases remain unchanged

Examples:
let a = GNot (GGlobally (GOr (GUntil (GAtom "a") (GAtom "b") ) (GNext (GAtom "c")) ))
let a = GNot (GExist 100 (ANot (AUntil (AAtom "a") (AAtom "b"))))

******************************************************************

***** PRETTY PRINTING ********************************************

For agent values:

> ppAVal :: AVal -> String
> ppAVal (ANumVal i)    = show i
> ppAVal (AAttribute s) = "att(" ++ show s ++ ")"
> ppAVal (ANumFunc s)   = show s ++ "()"
> ppAVal (AAsg k v)     = "(" ++ show k ++ ":=" ++ ppAVal v ++ ")"
> ppAVal (AVar k)       = show k ++ "?"
> ppAVal (APlus v1 v2)  = "(" ++ ppAVal v1 ++ " + " ++ ppAVal v2 ++ ")"
> ppAVal (AMinus v1 v2) = "(" ++ ppAVal v1 ++ " - " ++ ppAVal v2 ++ ")"
> ppAVal (ATimes v1 v2) = "(" ++ ppAVal v1 ++ " * " ++ ppAVal v2 ++ ")"

For group values:

> ppGVal :: GVal -> String
> ppGVal (GNumVal i)      = show i
> ppGVal (GNumFunc s)   = show s ++ "()"
> ppGVal (GAsg k v)     = "(" ++ show k ++ ":=" ++ ppGVal v ++ ")"
> ppGVal (GVar k)       = show k ++ "?"
> ppGVal (GPlus v1 v2)  = "(" ++ ppGVal v1 ++ " + " ++ ppGVal v2 ++ ")"
> ppGVal (GMinus v1 v2) = "(" ++ ppGVal v1 ++ " - " ++ ppGVal v2 ++ ")"
> ppGVal (GTimes v1 v2) = "(" ++ ppGVal v1 ++ " * " ++ ppGVal v2 ++ ")"

For agent formulae:

> ppALTL :: ALTL -> String
> ppALTL (AAtom s)        = show s
> ppALTL ATrue            = "True"
> ppALTL AFalse           = "False"
> ppALTL ALast            = "Last"
> ppALTL (ANot f)         = "!" ++ ppALTL f
> ppALTL (AAnd f1 f2)     = "(" ++ ppALTL f1 ++ " && " ++ ppALTL f2 ++ ")"
> ppALTL (AOr f1 f2)      = "(" ++ ppALTL f1 ++ " || " ++ ppALTL f2 ++ ")"
> ppALTL (AImpl f1 f2)    = "(" ++ ppALTL f1 ++ " -> " ++ ppALTL f2 ++ ")"
> ppALTL (AFinally f)     = "F " ++ ppALTL f
> ppALTL (AGlobally f)    = "G " ++ ppALTL f
> ppALTL (ANext f)        = "X " ++ ppALTL f
> ppALTL (ASNext f)       = "X! " ++ ppALTL f
> ppALTL (AUntil f1 f2)   = "(" ++ ppALTL f1 ++ " U " ++ ppALTL f2 ++ ")"
> ppALTL (ARelease f1 f2) = "(" ++ ppALTL f1 ++ " R " ++ ppALTL f2 ++ ")"
> ppALTL (AVal v)         = ppAVal v
> ppALTL (AEq v1 v2)      = "(" ++ ppAVal v1 ++ " == " ++ ppAVal v2 ++ ")"
> ppALTL (ANEq v1 v2)     = "(" ++ ppAVal v1 ++ " != " ++ ppAVal v2 ++ ")"
> ppALTL (AAEq d v1 v2)   = "(" ++ ppAVal v1 ++ " ~ " ++ ppAVal v2 ++ ")"
> ppALTL (ANAEq d v1 v2)  = "(" ++ ppAVal v1 ++ " !~ " ++ ppAVal v2 ++ ")"
> ppALTL (AGt v1 v2)      = "(" ++ ppAVal v1 ++ " > " ++ ppAVal v2 ++ ")"
> ppALTL (AGEq v1 v2)     = "(" ++ ppAVal v1 ++ " >= " ++ ppAVal v2 ++ ")"
> ppALTL (ALt v1 v2)      = "(" ++ ppAVal v1 ++ " < " ++ ppAVal v2 ++ ")"
> ppALTL (ALEq v1 v2)     = "(" ++ ppAVal v1 ++ " <= " ++ ppAVal v2 ++ ")"

For group formulae:

> ppGLTL :: GLTL -> String
> ppGLTL (GAtom s)        = show s
> ppGLTL GLast            = "Last"
> ppGLTL GTrue            = "True"
> ppGLTL GFalse           = "False"
> ppGLTL (GNot f)         = "!" ++ ppGLTL f
> ppGLTL (GAnd f1 f2)     = "(" ++ ppGLTL f1 ++ " && " ++ ppGLTL f2 ++ ")"
> ppGLTL (GOr f1 f2)      = "(" ++ ppGLTL f1 ++ " || " ++ ppGLTL f2 ++ ")"
> ppGLTL (GImpl f1 f2)    = "(" ++ ppGLTL f1 ++ " => " ++ ppGLTL f2 ++ ")"
> ppGLTL (GFinally f)     = "F " ++ ppGLTL f
> ppGLTL (GGlobally f)    = "G " ++ ppGLTL f
> ppGLTL (GNext f)        = "X! " ++ ppGLTL f
> ppGLTL (GSNext f)       = "X " ++ ppGLTL f
> ppGLTL (GUntil f1 f2)   = "(" ++ ppGLTL f1 ++ " U " ++ ppGLTL f2 ++ ")"
> ppGLTL (GRelease f1 f2) = "(" ++ ppGLTL f1 ++ " R " ++ ppGLTL f2 ++ ")"
> ppGLTL (GExist i f)     = "E[" ++ show i ++ "](" ++ ppGLTL f ++ ")"
> ppGLTL (GForall f)      = "A(" ++ ppGLTL f ++ ")"
> ppGLTL (GALTL f)        = ppALTL f
> ppGLTL (GSel f1 f2)     = "<< " ++ ppALTL f1 ++ ">>" ++ ppGLTL f2 
> ppGLTL (GEq v1 v2)      = "(" ++ ppGVal v1 ++ " == " ++ ppGVal v2 ++ ")"
> ppGLTL (GNEq v1 v2)     = "(" ++ ppGVal v1 ++ " != " ++ ppGVal v2 ++ ")"
> ppGLTL (GAEq d v1 v2)   = "(" ++ ppGVal v1 ++ " ~ " ++ ppGVal v2 ++ ")"
> ppGLTL (GNAEq d v1 v2)  = "(" ++ ppGVal v1 ++ " !~ " ++ ppGVal v2 ++ ")"
> ppGLTL (GGt v1 v2)      = "(" ++ ppGVal v1 ++ " > " ++ ppGVal v2 ++ ")"
> ppGLTL (GGEq v1 v2)     = "(" ++ ppGVal v1 ++ " >= " ++ ppGVal v2 ++ ")"
> ppGLTL (GLt v1 v2)      = "(" ++ ppGVal v1 ++ " < " ++ ppGVal v2 ++ ")"
> ppGLTL (GLEq v1 v2)     = "(" ++ ppGVal v1 ++ " <= " ++ ppGVal v2 ++ ")"

********************************************************************

Example queries:

let state1 = [("age",1)]
let state2 = [("age",2)]
let alh = [state1, state1, state1, state2]
let ltl = AFinally (AEq (AAttribute "age") (ANumVal 1)) 
checkAState alh ltl
