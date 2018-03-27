{-# LANGUAGE DeriveFunctor, ScopedTypeVariables, MultiParamTypeClasses, ConstraintKinds , RankNTypes, TypeOperators, FlexibleInstances #-}
module Twisted where

import Control.Applicative
import Control.Arrow
import Data.Monoid
import Foreign.Storable
import GHC.Ptr


data Foo a = Bar Int | Baz a deriving (Show, Functor)

data Lst a = Lst {unLst :: forall i. Monoid i => (a -> i) -> i}

type (~>) f g = forall i. f i -> g i
newtype Free1 s f a = Free1 { gratis1 :: forall g. s g => (f ~> g) -> g a}

isou :: (Functor f) => Free1 Functor f (Fix f) -> Fix f
isou (Free1 f) = In $ f id

isoc :: (Functor f) => Fix f -> Free1 Functor f (Fix f)
isoc (In f) = Free1 $ \fg -> fg f  

ss :: Foo a -> Free1 Functor Foo a 
ss t = Free1 $ \f -> f t

shit :: Free1 Functor f a -> Free1 Functor f a
shit (Free1 fg) = Free1 $ \mg -> fg mg  

kons :: a -> Lst a -> Lst a
kons x (Lst f) = Lst $ f <> (\g -> g x)

data Fix f = In {in0 :: f (Fix f)} 
------------------------------------------------------
 
class Functor f => Monoidal f where
    unit :: f ()
    (>*<) :: f a -> f b -> f (a,b)

class Monoid m => Action m a where 
    (<.>) :: m -> a -> a

data a <#> m = a :# m

unSemi :: a <#> m -> (a,m)
unSemi (a :# m) = (a,m)

semi :: (a, m) -> a <#> m
semi (a,m) = a :# m

instance (Monoid m, Monoid a, Action m a) => Monoid (a <#> m) where 
    mempty = mempty :# mempty
    mappend (a :# m) (b :# n) = semi $ (a <> (m <.> b), m <> n)

----------------------------------------------------------
class (Monoid m, Functor f) => ActionF m f where
    (<..>) :: m -> f a -> f a

instance Monoidal ((->) a) where 
    unit = const ()
    f >*< g = f &&& g

instance (Action m a, Monoid m) => ActionF m ((->) a) where 
    m <..> ab = ab . (m <.>)  

-------------------------------------------------------------
instance Arrow a => Monoidal (WrappedArrow a b) where 
    unit = WrapArrow $ arr (const ())
    WrapArrow f >*< WrapArrow g = WrapArrow $ f &&& g
    
instance (Monoid m, Arrow a, Action m b) => ActionF m (WrappedArrow a b) where
    m <..> (WrapArrow abc) = WrapArrow $ abc <<< arr (m <.>)

-----------------------------------------------------------

newtype WriteAction = WA(forall a. Ptr a -> IO ())
