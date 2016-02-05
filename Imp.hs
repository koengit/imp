module Imp where

import Data.List
import System.Cmd
import Test.QuickCheck
import Test.QuickCheck.Monadic

type V = String

data P
  = Block [V] P
  | P :>> P
  | While E P
  | If E P P
  | V := E
  | Skip
 deriving ( Eq, Ord, Show )

instance Arbitrary P where
  arbitrary = sized arbP
   where
    arbP n = frequency
      [ (n, do xs <- nub `fmap` listOf arbV
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
  shrink Skip         = []


free :: P -> [V]
free (Block xs p) = free p \\ xs
free (p :>> q)    = free p `union` free q
free (While e p)  = freeE e `union` free p
free (If e p q)   = freeE e `union` free p `union` free q
free (x := e)     = [x] `union` freeE e
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
showP Skip         = []

nest :: Int -> [String] -> [String]
nest k = map (replicate k ' ' ++)

data E
  = Var V
  | Int Integer
  | E :+: E
 deriving ( Eq, Ord )

instance Arbitrary E where
  arbitrary = sized arbE
   where
    arbE n = frequency
      [ (n, do a <- arbE n2
               b <- arbE n2
               return (a :+: b))
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
  shrink (Var x)   = [Int 0, Int 1]
  shrink (Int n)   = [Int n' | n' <- shrink n]

arbV = growingElements [ "x" ++ show i | i <- [1..15] ]

freeE :: E -> [V]
freeE (Var x)   = [x]
freeE (Int _)   = []
freeE (a :+: b) = freeE a `union` freeE b

instance Show E where
  show (Var x)   = x
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
       ++ nest 2 (showP p)
       ++ [ ""
          , "  /* printing free variables */"
          ]
       ++ [ "  printf(\"STATE " ++ x ++ " = %d\\n\", " ++ x ++ ");"
          | x <- xs
          ]
       ++ [ "}" ]
     system "gcc program.c"
     system "./a.out > output"
     s <- readFile "output"
     let st' = [ (x,read v) | ["STATE",x,"=",v] <- map words (lines s) ]
     length st' `seq` return st'
 where
  xs = map fst st          

p = "x" := (Var "x" :+: Int 1)
st = [("x",3)]

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

arbState :: [V] -> Gen State
arbState xs =
  do vs <- sequence [ arbitrary | x <- xs ]
     return (xs `zip` vs)

