{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE Unsafe #-}

module Signal
where

data Signal a = a :- (Signal a)
    deriving (Show, Eq)

now :: Signal a -> a
now (h :- t) = h

next :: Signal a -> Signal a
next (h :- t) = t

register :: a -> Signal a -> Signal a
register df input = df :- input

mealy :: (s -> i -> (s, o)) -> s -> Signal i -> Signal o
mealy f s (h :- t) = o' :- (mealy f s' t)
    where
        (s', o') = f s h

zipS :: (a -> b -> c) -> Signal a -> Signal b -> Signal c
zipS f (hl:-tl) (hr:-tr) = (f hl hr) :- (zipS f tl tr)

addS :: Signal Integer -> Signal Integer -> Signal Integer
addS = zipS (+)

subS :: Signal Integer -> Signal Integer -> Signal Integer
subS = zipS (-)

direct_integrator :: Signal Integer -> Signal Integer
direct_integrator i = mealy (\(a1, a2) b -> ((b, a1), (a1 + a2))) (0,0) i

{-# RULES 
"morgans law" forall i. register (0 :: Integer) (direct_integrator i) = direct_integrator (register (0 :: Integer) i)
#-}