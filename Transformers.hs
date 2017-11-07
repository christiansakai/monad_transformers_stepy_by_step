{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Transformers where

import Control.Monad.Identity 
  ( Identity
  , runIdentity
  )
import Control.Monad.Except
  ( ExceptT
  , runExceptT
  )
import Control.Monad.Trans.Except
  ( throwE 
  )
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

{-|
  Data
-}
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


{-|
  Basic non-Monadic evaluation
-}
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

{-|
  Example expression
-}
exampleValidExp :: Exp
exampleValidExp = 
  Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

exampleInvalidExp :: Exp
exampleInvalidExp =
  Lit 1 `Plus` (Abs "x" (Var "x"))


{-|
  Typeclass instances for Monadic expression
-}
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


{-|
  Monadic evaluation, also not using do notation
-}
eval1 :: Monad m => Env -> Exp -> m Value
eval1 env (Lit i)       = return $ IntVal i 
eval1 env (Var n)       = return $ fromJust $ Map.lookup n env
eval1 env (Plus e1 e2)  = 
  eval1 env e1 >>= \(IntVal i1) ->
    eval1 env e2 >>= \(IntVal i2) ->
      return $ IntVal $ i1 + i2
eval1 env (Abs n e)     = return $ FunVal env n e
eval1 env (App e1 e2)  = 
  eval1 env e1 >>= \v1 ->
    eval1 env e2 >>= \v2 ->
      case v1 of
        FunVal env' n body ->
          let env'' = Map.insert n v2 env'
           in eval1 env'' body
 
{-|
  Typeclass instances for ExceptT
-}
type Eval2 a = ExceptT String Identity a

runEval2 :: forall a . Eval2 a -> Either String a
runEval2 eval2 = 
  let idEither = runExceptT eval2   :: Identity (Either String a)
      either = runIdentity idEither :: Either String a
   in either

{-|
  ExceptT monad evaluation. Not using the exception yet.
  From here on below, uses do notation for Monad
-}
eval2a :: Env -> Exp -> Eval2 Value
eval2a env (Lit i)         = return $ IntVal i
eval2a env (Var n)         = return $ fromJust $ Map.lookup n env
eval2a env (Plus e1 e2)    = do
  IntVal i1 <- eval2a env e1
  IntVal i2 <- eval2a env e2
  return $ IntVal (i1 + i2)
eval2a env (Abs n e)       = return $ FunVal env n e
eval2a env (App e1 e2)     = do
  v1 <- eval2a env e1
  v2 <- eval2a env e2
  case v1 of
    FunVal env' n body ->
      let env'' = Map.insert n v2 env'
       in eval2a env'' body

{-|
  ExceptT monad evaluation. Using the exception.
-}
eval2b :: Env -> Exp -> Eval2 Value
eval2b env (Lit i)        = return $ IntVal i
eval2b env (Var n)        = 
  case Map.lookup n env of
    Just v -> return v
    _      -> throwE "Data.Map.lookup: Key not found"
eval2b env (Plus e1 e2)   = do
  v1 <- eval2b env e1
  v2 <- eval2b env e2
  case (v1, v2) of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _                      -> throwE "type error"
eval2b env (Abs n e)      = return $ FunVal env n e
eval2b env (App e1 e2)    = do
  v1 <- eval2b env e1
  v2 <- eval2b env e2
  case v1 of
    FunVal env' n body ->
      let env'' = Map.insert n v2 env'
       in eval2b env'' body
    _                  ->
      throwE "type error"

{-|
  Better error messages
-}
eval2 :: Env -> Exp -> Eval2 Value
eval2 env (Lit i)         = return $ IntVal i
eval2 env (Var n)         =
  case Map.lookup n env of
    Nothing -> throwE ("unbound variable: " ++ n)
    Just val  -> return val
eval2 env (Plus e1 e2)    = do
  v1 <- eval2 env e1
  v2 <- eval2 env e2
  case (v1, v2) of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _                      -> throwE "type error in addition"
eval2 env (Abs n e)       = return $ FunVal env n e
eval2 env (App e1 e2)     = do
  v1 <- eval2 env e1
  v2 <- eval2 env e2
  case v1 of
    FunVal env' n body ->
      let env'' = Map.insert n v2 env'
       in eval2 env'' body
    _                  ->
      throwE "type error in application"




