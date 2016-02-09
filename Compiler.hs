module Compiler where

import Data.Map( Map )
import qualified Data.Map as M
import Data.List( union )
import System.Process

import Imp

--------------------------------------------------------------------------------
-- compiler

crun :: S -> P -> IO S
crun st p =
  do writeFile "program.c" $ unlines $
          [ "#include <stdio.h>"
          , ""
          , "void main() {"
          , "  /* initializing free variables */"
          ]
       ++ [ "  int " ++ show x ++ " = " ++ show v ++ ";"
          | (x,v) <- M.toList st
          ]
       ++ [ ""
          , "  /* the program */"
          ]
       ++ nest 2 (showP p)
       ++ [ ""
          , "  /* printing free variables */"
          ]
       ++ [ "  printf(\"STATE " ++ show x ++ " = %d\\n\", " ++ show x ++ ");"
          | x <- xs
          ]
       ++ [ "}" ]
     system "gcc program.c"
     system "( ulimit -t 1 ; ./a.out > output )"
     s <- readFile "output"
     let st' = M.fromList [ (V x,read v) | ["STATE",x,"=",v] <- map words (lines s) ]
     st' `seq` return st'
 where
  xs = M.keys st          

--------------------------------------------------------------------------------
-- buggy compilers

crun1 :: S -> P -> IO S
crun1 st p = crun st (trans [] p)
 where
  trans zs (Block xs p) =
    Block (filter (`notElem` zs) xs) (trans (zs `union` xs) p)
  
  trans zs (p :>> q)   = trans zs p :>> trans zs q
  trans zs (While e p) = While e (trans zs p)
  trans zs (If e p q)  = If e (trans zs p) (trans zs q)
  trans zs p           = p 

--------------------------------------------------------------------------------

