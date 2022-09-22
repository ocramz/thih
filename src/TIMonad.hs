-----------------------------------------------------------------------------
-- TIMonad:	Type inference monad
-- 
-- Part of `Typing Haskell in Haskell', version of November 23, 2000
-- Copyright (c) Mark P Jones and the Oregon Graduate Institute
-- of Science and Technology, 1999-2000
-- 
-- This program is distributed as Free Software under the terms
-- in the file "License" that is included in the distribution
-- of this software, copies of which may be obtained from:
--             http://www.cse.ogi.edu/~mpj/thih/
-- 
-----------------------------------------------------------------------------
{-# language DeriveFunctor #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# options_ghc -Wno-unused-imports #-}
module TIMonad where
import Id (enumId)
import Kind
import Type
import Subst
import Unify
import Pred (Pred(..), Qual(..))
import Scheme

import Data.Bifunctor (first, second)

-- mtl
import Control.Monad.State (MonadState(..), gets, modify)
-- transformers
import Control.Monad.Trans.State (StateT, evalStateT)



newtype TI a = TI (StateT (Subst, Int) (Either String) a) deriving (Functor, Applicative, Monad, MonadState (Subst, Int))

instance MonadFail TI where
  fail = error -- BOOM -- FIXME

runTI :: TI a -> Either String a
runTI (TI f) = flip evalStateT (nullSubst, 0) f

getSubst :: TI Subst
getSubst = gets fst

unify      :: Type -> Type -> TI ()
unify t1 t2 = do
  s <- getSubst
  u <- mgu (apply s t1) (apply s t2)
  extSubst u

trim       :: [Tyvar] -> TI ()
trim vs = do
  (s, n) <- get
  let
    s' = [ (v,t) | (v,t) <- s, v `elem` vs ]
    force = length (tv (map snd s'))
    _ = force `seq` s'
  put (s', n)

extSubst :: Subst -> TI ()
extSubst s' = modify (first (\s -> s' @@ s))

newTVar    :: Kind -> TI Type
newTVar k = do
  (s, n) <- get
  let
    v = Tyvar (enumId n) k
  put (s, n+1)
  pure (TVar v)

freshInst               :: Scheme -> TI (Qual Type)
freshInst (Forall ks qt) = do ts <- mapM newTVar ks
                              return (inst ts qt)

class Instantiate t where
  inst  :: [Type] -> t -> t
instance Instantiate Type where
  inst ts (TAp l r) = TAp (inst ts l) (inst ts r)
  inst ts (TGen n)  = ts !! n
  inst _  t         = t
instance Instantiate a => Instantiate [a] where
  inst ts = map (inst ts)
instance Instantiate t => Instantiate (Qual t) where
  inst ts (ps :=> t) = inst ts ps :=> inst ts t
instance Instantiate Pred where
  inst ts (IsIn c t) = IsIn c (inst ts t)

-----------------------------------------------------------------------------



-- newtype TI a = TI (Subst -> Int -> (Subst, Int, a)) 

-- runTI       :: TI a -> a
-- runTI (TI f) = x where (s,n,x) = f nullSubst 0

-- instance Monad TI where
--   return x   = TI (\s n -> (s,n,x))
--   TI f >>= g = TI (\s n -> case f s n of
--                             (s',m,x) -> let TI gx = g x
--                                         in  gx s' m)

-- newTVar    :: Kind -> TI Type
-- newTVar k   = TI (\s n -> let v = Tyvar (enumId n) k
--                           in  (s, n+1, TVar v))

-- unify      :: Type -> Type -> TI ()
-- unify t1 t2 = do s <- getSubst
--                  u <- mgu (apply s t1) (apply s t2)
--                  extSubst u

-- getSubst   :: TI Subst
-- getSubst    = TI (\s n -> (s,n,s))


-- trim       :: [Tyvar] -> TI ()
-- trim vs     = TI (\s n ->
--                   let s' = [ (v,t) | (v,t) <-s, v `elem` vs ]
--                       force = length (tv (map snd s'))
--                   in  force `seq` (s', n, ()))


-- extSubst   :: Subst -> TI ()
-- extSubst s' = TI (\s n -> (s'@@s, n, ()))
