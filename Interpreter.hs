module Interpreter where

import Data.Map( Map, (!), insert, union )
import qualified Data.Map as M

import Imp

--------------------------------------------------------------------------------
-- interpreter

-- expressions

interpretE :: E -> S -> (Integer, S)
interpretE (Var v) e = (e ! v, e)
interpretE (Int i) e = (i, e)
interpretE (l :+: r) e = 
    let (li, e' ) = interpretE l e
        (ri, e'') = interpretE r e'
    in (li + ri, e'')
interpretE (Inc v) e = (e ! v , M.adjust (+ 1) v e)

initVars :: [V] -> S
initVars vars = M.fromList $ fmap (\x -> (x,0)) vars

-- programs

interpret :: P -> S -> S
interpret (Block vars p) e = oldValsMap `union` resMap
   where resMap =  interpret p (initVars vars `union` e)
         oldVars = filter (`M.member` e) vars
         oldValsMap = M.fromList $ zip oldVars (map (e !) oldVars)
interpret (f :>> g)      e = interpret g (interpret f e)
interpret (While c b)    e = 
    let (cv, e') = interpretE c e
    in if cv /= 0
       then interpret (b :>> While c b) e'
       else e'
interpret (If c t f) e = 
    let (cv, e') = interpretE c e
    in if cv /= 0
       then interpret t e'
       else interpret f e'
interpret (v := exp) e = 
    let (i, e') = interpretE exp e
    in insert v i e'
interpret Skip e = e
interpret Break e = e -- break = skip
  
--------------------------------------------------------------------------------

