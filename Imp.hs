module Imp where

import Data.List
import Data.Map( Map, (!) )
import qualified Data.Map as M
import Test.QuickCheck

--------------------------------------------------------------------------------
-- variables

newtype V = V String
 deriving ( Eq, Ord )

instance Show V where
  show (V x) = x

instance Arbitrary V where
  arbitrary = growingElements [ V ("x" ++ show i) | i <- [1..15] ]
  shrink _  = []

newtype Vars = Vars [V]
 deriving ( Eq, Ord )

instance Show Vars where
  show (Vars xs) = show xs

instance Arbitrary Vars where
  arbitrary        = (Vars . nub) `fmap` arbitrary
  shrink (Vars xs) = [ Vars (nub xs') | xs' <- shrinkList (\_ -> []) xs ]

--------------------------------------------------------------------------------
-- expressions

data E
  = Var V
  | Int Integer
  | E :+: E
  | Inc V
 deriving ( Eq, Ord )

instance Show E where
  show (Var x)   = show x
  show (Inc x)   = "(" ++ show x ++ "++)"
  show (Int n)   = show n
  show (a :+: b) = "(" ++ show a ++ "+" ++ show b ++ ")"

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
      , (1, do v <- arbitrary
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

freeE :: E -> [V]
freeE (Var x)   = [x]
freeE (Inc x)   = [x]
freeE (Int _)   = []
freeE (a :+: b) = freeE a `union` freeE b

--------------------------------------------------------------------------------
-- programs

data P
  = Block [V] P
  | P :>> P
  | While E P
  | If E P P
  | V := E
  | Skip
  | Break
 deriving ( Eq, Ord, Show )

instance Arbitrary P where
  arbitrary = sized arbP
   where
    arbP n = frequency
      [ (n, do --Vars xs <- arbitrary
               x <- arbitrary
               p <- arbP n1
               return (Block [x] p))
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
      , (3, do x <- arbitrary
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
                  ++ nest 2 [ "int " ++ show x ++ " = 0;" | x <- xs ]
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
showP (x := e)     = [ show x ++ " = " ++ show e ++ ";" ]
showP Break        = [ "break;" ]
showP Skip         = []

nest :: Int -> [String] -> [String]
nest k = map (replicate k ' ' ++)

--------------------------------------------------------------------------------
-- states

type S = Map V Integer

arbS :: [V] -> Gen S
arbS xs =
  do vs <- sequence [ arbitrary | x <- xs ]
     return (M.fromList (xs `zip` vs))

shrinkS :: S -> [S]
shrinkS st = [ M.insert x v' st | x <- M.keys st, v' <- shrink (st ! x) ]

--------------------------------------------------------------------------------

