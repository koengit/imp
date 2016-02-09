module Main where

import Data.Map( Map, (!), insert, union )
import qualified Data.Map as M
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Imp
import Compiler
import Interpreter

--------------------------------------------------------------------------------
-- direct testing against interpreter

prop_Compile_Interpret crun p =
  forAllShrink (arbS xs) shrinkS $ \st ->
    monadicIO $
      do st1 <- run (crun st p)
         let st2 = interpret p st
         monitor $ whenFail (print (st1,st2))
         assert (st1 == st2)
 where
  xs = free p

--------------------------------------------------------------------------------
-- inductive testing

prop_Compile_Inductive crun p =
  forAllShrink (arbS xs) shrinkS $ \st ->
    monadicIO $
      do st1 <- run (crun st p)
         st2 <- crunInd crun st p
         monitor $ whenFail' (writeFile "code.c" (unlines (showP p)))
         assert (st1 == st2)
 where
  xs = free p

crunInd :: (S -> P -> IO S) -> S -> P -> PropertyM IO S
crunInd crun st (While e p) =
  run $ crun st (If e (p :>> While e p) Skip)

crunInd crun st (If e p q) =
  run $
    do stb <- crun (insert b 0 st) (b := e)
       if stb ! b /= 0 then
         crun st p
        else
         crun st q
 where
  b = V "b"

crunInd crun st (p :>> q) =
  run $
    do st1 <- crun st p
       crun st1 q

crunInd crun st (Block xs p) =
  run $
    do st' <- crun (M.fromList (xs `zip` repeat 0) `M.union` st) p
       return (M.filterWithKey (\x _ -> x `notElem` xs) st' `M.union`
               M.filterWithKey (\x _ -> x `elem` xs) st)

-- rest is not implemented
crunInd _ _ _ =
  do pre False
     return undefined

--------------------------------------------------------------------------------

main = quickCheckWith (stdArgs { maxSuccess = 10000 }) $ prop_Compile_Interpret crun1

--------------------------------------------------------------------------------

