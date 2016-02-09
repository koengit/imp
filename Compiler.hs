module Compiler where

import Data.Map( Map )
import qualified Data.Map as M
import Data.List( union, intersect )
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
       ++ [ "  int " ++ show x ++ ";"
          | x <- xs
          ]
       ++ [ "  scanf(\"%d\", &" ++ show x ++ ");"
          | x <- xs
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
     writeFile "input" $ unwords [ show v | (_,v) <- M.toList st ]
     system "gcc program.c 2> /dev/null"
     system "timeout 0.2s ./a.out <input > output"
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
    Block (filter (`notElem` zs) xs)
          (foldr (:>>) (trans (zs `union` xs) p) [ x := Int 0 | x <- zs `intersect` xs ])
  
  trans zs (p :>> q)   = trans zs p :>> trans zs q
  trans zs (While e p) = While e (trans zs p)
  trans zs (If e p q)  = If e (trans zs p) (trans zs q)
  trans zs p           = p 

--------------------------------------------------------------------------------

