{-# LANGUAGE RankNTypes #-}
module TwistedRomulo where


import Control.Applicative
import Control.Arrow
import Data.Monoid
import Foreign.Storable
import GHC.Ptr


------------- Data Serialization -------------------
newtype WriteAction = WA(forall a. Ptr a -> IO ())

instance Monoid WriteAction where 
    mempty = WA $ const (return())
    WA w1 `mappend` WA w2 = WA $ \ptr -> w1 ptr >> w2 ptr

------------- Deserialization -------------------
newtype ParseAction a = PA (forall p. Ptr p -> IO a)

instance Functor ParseAction where
    fmap f (PA px) = PA (\ptr -> fmap f (px ptr)) 

instance Applicative ParseAction where
    pure p = PA ( const ( return p ))
    PA pf <*> PA px = PA $ \ptr -> pf ptr <*> px ptr

data Parser a = P (ParseAction a) (Sum Int)

-- type ParseAction' = Compose ((->) (forall p. Ptr p) ) IO
{-
instance Applicative Parser where
pure a = P (pure a)
P pf n1 <*> P px n2 = P (pf <*> (n1 ? px)) (n1 + n2 )
-}

