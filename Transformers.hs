{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Transformers where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

type Name = String

data Exp =
    Lit Integer       -- literal integers (constants)
  | Var Name          -- variables
  | Plus Exp Exp      -- addition
  | Abs Name Exp      -- lambda expressions (abstractions)
  | App Exp Exp       -- function application
  deriving Show

data Value =
    IntVal Integer        -- integers 
  | FunVal Env Name Exp   -- function application
  deriving Show

type Env = Map.Map Name Value

eval0 :: Env -> Exp -> Value
eval0 env (Lit i)       = IntVal i
eval0 env (Var n)       = fromJust $ Map.lookup n env
eval0 env (Plus e1 e2)  = 
  let IntVal i1 = eval0 env e1
      IntVal i2 = eval0 env e2
   in IntVal $ i1 + i2
eval0 env (Abs n e)     = FunVal env n e
eval0 env (App e1 e2)   = 
  let v1 = eval0 env e1
      v2 = eval0 env e2
   in case v1 of
        FunVal env' n body ->
          let env'' = Map.insert n v2 env'
           in eval0 env'' body

exampleExp :: Exp
exampleExp = 
  Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

newtype Eval1 a = Eval1 { runEval1 :: a }

instance Functor Eval1 where
  fmap :: (a -> b) -> Eval1 a -> Eval1 b
  fmap f = Eval1 . f . runEval1

instance Applicative Eval1 where
  pure :: a -> Eval1 a
  pure = Eval1

  (<*>) :: Eval1 (a -> b) -> Eval1 a -> Eval1 b 
  evF <*> evA = Eval1 . f $ a
    where f = runEval1 evF
          a = runEval1 evA

instance Monad Eval1 where
  return :: a -> Eval1 a
  return = pure

  (>>=) :: Eval1 a -> (a -> Eval1 b) -> Eval1 b
  evA >>= f = f . runEval1 $ evA  

eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i)       = return $ IntVal i 
eval1 env (Var n)       = return $ fromJust $ Map.lookup n env
eval1 env (Plus e1 e2)  =
  let IntVal i1 = runEval1 $ eval1 env e1
      IntVal i2 = runEval1 $ eval1 env e2
   in return $ IntVal $ i1 + i2
eval1 env (Abs n e)     = return $ FunVal env n e
eval1 env (App e1 e2)  =
  let v1 = runEval1 $ eval1 env e1
      v2 = runEval1 $ eval1 env e2
   in case v1 of
        FunVal env' n body ->
          let env'' = Map.insert n v2 env'
           in eval1 env'' body
