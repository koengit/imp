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

prop_Compile_Interpret p =
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

{-
prop_Compile_Inductive p =
  forAllShrink (arbS xs) shrinkS $ \st ->
    monadicIO $
      do st1 <- run (crun1 st p)
         st2 <- run (crunInd st p)
         assert (st1 == st2)
 where
  xs = free p

crunInd :: S -> P -> IO S
crunInd st (While e p) =
  crun st (If e (p :>> While e p) Skip)

crunInd st (If e p) =
  crun st (If e (p :>> While e p) Skip)

crunInd 
-}

--------------------------------------------------------------------------------

{-
import Data.List hiding (map,insert)
import System.Cmd
import Data.Map hiding (map, union, (\\),filter)
import qualified Data.Map as Map

newtype V = V String

data P
  = Block [V] P
  | P :>> P
  | While E P
  | If E P P
  | V := E
  | Skip
  | Break
 deriving ( Eq, Ord, Show )

newtype Vars = Vars [V] deriving ( Eq, Ord, Show )

instance Arbitrary Vars where
  arbitrary        = (Vars . nub) `fmap` listOf arbV
  shrink (Vars xs) = [ Vars (nub xs') | xs' <- shrinkList (\_ -> []) xs ]

instance Arbitrary P where
  arbitrary = sized arbP
   where
    arbP n = frequency
      [ (n, do Vars xs <- arbitrary
               p <- arbP n1
               return (Block xs p))
      , (n, do p <- arbP n2
               q <- arbP n2
               return (p :>> q))
      --, (n, do e <- arbitrary
      --         p <- arbP n1
      --         return (While e p))
      , (n, do e <- arbitrary
               p <- arbP n2
               q <- arbP n2
               return (If e p q))
      , (3, do x <- arbV
               e <- arbitrary
               return (x := e))
      , (1, do return Skip)
      -- , (1, do return Break)
      ]
     where
      n2 = n `div` 2
      n1 = n-1

  shrink (Block xs p) = [p]
                     ++ [ Block xs' p | xs' <- shrink xs ]
                     ++ [ Block xs p' | p' <- shrink p ]
  shrink (p :>> q)    = [p,q]
                     ++ [ p' :>> q | p' <- shrink p ]
                     ++ [ p :>> q' | q' <- shrink q ]
  shrink (While e p)  = [p, If e p Skip]
                     ++ [ While e' p | e' <- shrink e ]
                     ++ [ While e p' | p' <- shrink p ]
  shrink (If e p q)   = [p,q]
                     ++ [ If e' p q | e' <- shrink e ]
                     ++ [ If e p' q | p' <- shrink p ]
                     ++ [ If e p q' | q' <- shrink q ]
  shrink (x := e)     = [Skip]
                     ++ [ x := e' | e' <- shrink e ]
  shrink Break        = [Skip]
  shrink Skip         = []


free :: P -> [V]
free (Block xs p) = free p \\ xs
free (p :>> q)    = free p `union` free q
free (While e p)  = freeE e `union` free p
free (If e p q)   = freeE e `union` free p `union` free q
free (x := e)     = [x] `union` freeE e
free Break        = []
free Skip         = []

showP :: P -> [String]
showP (Block xs p) = [ "{" ]
                  ++ nest 2 [ "int " ++ x ++ " = 0;" | x <- xs ]
                  ++ nest 2 (showP p)
                  ++ [ "}" ]
showP (p :>> q)    = showP p ++ showP q
showP (While e p)  = [ "while (" ++ show e ++ ") {" ]
                  ++ nest 2 (showP p)
                  ++ [ "}" ]
showP (If e p q)   = [ "if (" ++ show e ++ ") {" ]
                  ++ nest 2 (showP p)
                  ++ [ "} else {" ]
                  ++ nest 2 (showP q)
                  ++ [ "}" ]
showP (x := e)     = [ x ++ " = " ++ show e ++ ";" ]
showP Break        = [ "break;" ]
showP Skip         = []

nest :: Int -> [String] -> [String]
nest k = map (replicate k ' ' ++)

data E
  = Var V
  | Int Integer
  | E :+: E
  | Inc V
 deriving ( Eq, Ord )

instance Arbitrary E where
  arbitrary = sized arbE
   where
    arbE n = frequency
      [ (n, do a <- arbE n2
               b <- arbE n2
               return (a :+: b))
      --, (1, do v <- arbV
      --         return (Inc v))
      , (1, do n <- arbitrary
               return (Int n))
      , (1, do v <- arbV
               return (Var v))
      ]
     where
      n2 = n `div` 2
      n1 = n-1

  shrink (a :+: b) = [a,b]
                  ++ [ a' :+: b | a' <- shrink a ]
                  ++ [ a :+: b' | b' <- shrink b ]
  shrink (Inc x)   = [Var x]
  shrink (Var x)   = [Int 0, Int 1]
  shrink (Int n)   = [Int n' | n' <- shrink n]

arbV = growingElements [ "x" ++ show i | i <- [1..15] ]

freeE :: E -> [V]
freeE (Var x)   = [x]
freeE (Inc x)   = [x]
freeE (Int _)   = []
freeE (a :+: b) = freeE a `union` freeE b

instance Show E where
  show (Var x)   = x
  show (Inc x)   = x ++ "++"
  show (Int n)   = show n
  show (a :+: b) = "(" ++ show a ++ "+" ++ show b ++ ")"

type State = [(V,Integer)]

crun :: State -> P -> IO State
crun st p =
  do writeFile "program.c" $ unlines $
          [ "#include <stdio.h>"
          , ""
          , "void main() {"
          , "  /* initializing free variables */"
          ]
       ++ [ "  int " ++ x ++ " = " ++ show v ++ ";"
          | (x,v) <- st
          ]
       ++ [ ""
          , "  /* the program */"
          ]
       ++ nest 2 (showP (trans [] p))
       ++ [ ""
          , "  /* printing free variables */"
          ]
       ++ [ "  printf(\"STATE " ++ x ++ " = %d\\n\", " ++ x ++ ");"
          | x <- xs
          ]
       ++ [ "}" ]
     system "gcc program.c"
     system "( ulimit -t 1 ; ./a.out > output )"
     s <- readFile "output"
     let st' = [ (x,read v) | ["STATE",x,"=",v] <- map words (lines s) ]
     length st' `seq` return st'
 where
  xs = map fst st          

  trans zs (Block xs p) =
    Block (filter (`notElem` zs) xs) (trans (zs `union` xs) p)
  
  trans zs (p :>> q)   = trans zs p :>> trans zs q
  trans zs (While e p) = While e (trans zs p)
  trans zs (If e p q)  = If e (trans zs p) (trans zs q)
  trans zs p           = p 

p = "x" := (Var "x" :+: Int 1)
st = [("x",3)]

arbState :: [V] -> Gen State
arbState xs =
  do vs <- sequence [ arbitrary | x <- xs ]
     return (xs `zip` vs)

prop_Seq p q =
  monadicIO $
    do st1  <- pick (arbState xs)
    
       -- running p;q
       st3a <- run (crun st1 (p :>> q))
       
       -- running p and q    
       st2  <- run (crun st1 p)
       st3b <- run (crun st2 q)
       
       assert (st3a == st3b)
 where
  xs = free p `union` free q

prop_While e p =
  monadicIO $
    do st1  <- pick (arbState xs)
    
       -- running while(e)p
       st2a <- run (crun st1 (While e p))
       
       -- running p and q    
       st2b <- run (crun st1 (If e (p :>> While e p) Skip))
       
       assert (st2a == st2b)
 where
  xs = freeE e `union` free p

prop_If e p q =
  monadicIO $
    do st1  <- pick (arbState xs)
    
       -- running if(e)pq
       st3a <- run (crun st1 (If e p q))
       
       -- running p and q    
       st2  <- run (crun (("b",0):st1) ("b" := e))
       st3b <- run (crun st1 (if head [ v | ("b",v) <- st2 ] /= 0 then p else q))

       assert (st3a == st3b)
 where
  xs = freeE e `union` free p `union` free q

prop_Block (Vars ys) p =
  monadicIO $
    do st1  <- pick (arbState xs)
    
       st2a <- run (crun st1 (Block ys p))
       st2b <- run (crun ((ys `zip` repeat 0) ++ filter ((`notElem` ys).fst) st1) p)
       
       monitor $ whenFail (print st2a)
       monitor $ whenFail (print st2b)
       
       assert (sort st2a == sort ( filter ((`elem` ys).fst) st1
                                ++ filter ((`notElem` ys).fst) st2b
                                 ))
 where
  xs = free p `union` ys




type Env = Map V Integer

interpretE :: E -> Env -> (Integer, Env)
interpretE (Var v) e = (e ! v, e)
interpretE (Int i) e = (i, e)
interpretE (l :+: r) e = 
    let (li, e' ) = interpretE l e
        (ri, e'') = interpretE r e'
    in (li + ri, e'')
interpretE (Inc v) e = (e ! v , adjust (+ 1) v e)

initVars :: [V] -> Env
initVars vars = fromList $ fmap (\x -> (x,0)) vars

interpret :: P -> Env -> Env
interpret (Block vars p) e = interpret p (initVars vars `Map.union` e)
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
  
propInterpreterSane :: P -> Bool
propInterpreterSane p = 
   let res = interpret p (initVars (free p))
   in length (toList res) `seq` True

      
-}



