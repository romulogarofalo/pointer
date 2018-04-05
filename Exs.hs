{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Exs where

{-
1) mostre que V=R3 (Double,Double,Double), F=R

2) Implemente um TypeClass Espaco Vetorial

-}

instance Espaco Double where
    (⨂) a (x,y,z) = ((a*x),(a*y),(a*z))
    (⨁) (a,b,c) (x,y,z) = ((a+x),(b+y),(c+z))



class Espaco v f where
    (⨂) :: f -> v -> v
    (⨁) :: v -> v -> v
