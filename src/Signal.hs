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
{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}

module Signal
where

import Data.List


data Signal a = a :- (Signal a)
    deriving (Show, Eq)

sRepeat :: a -> Signal a
sRepeat i = i :- sRepeat i

fromList :: [a] -> Signal a
fromList (x : xs) = x :- fromList xs

toList :: Signal a -> [a]
toList (x :- xs) = x : toList xs

now :: Signal a -> a
now (h :- _) = h

next :: Signal a -> Signal a
next (h :- t) = t

register :: a -> Signal a -> Signal a
register df input = df :- input

mealy :: (s -> i -> (s, o)) -> s -> Signal i -> Signal o
mealy f s (h :- t) = o' :- mealy f s' t
    where
        (s', o') = f s h

zipS :: (a -> b -> c) -> Signal a -> Signal b -> Signal c
zipS f (hl:-tl) (hr:-tr) = f hl hr :- zipS f tl tr

addS :: Signal Integer -> Signal Integer -> Signal Integer
addS = zipS (+)

subS :: Signal Integer -> Signal Integer -> Signal Integer
subS = zipS (-)

directIntegrator :: Signal Integer -> Signal Integer
directIntegrator = mealy (\(a1, a2, a3, a4) b -> ((b, a1, a2, a3), a1 + a2 + a3 + a4)) (0,0,0,0)

directIntegrator2 :: Signal Integer -> Signal Integer
directIntegrator2 = mealy (\(acc, a1, a2, a3, a4) b -> ((acc + b - a4,b, a1, a2, a3), acc)) (0,0,0,0,0)


{-# RULES
"morgans law 5" forall i. take 5 $ toList $ directIntegrator i = take 5 $ toList $ directIntegrator2 i
#-}

{-# RULES
"morgans law 4" forall i. take 4 $ toList $ directIntegrator i = take 4 $ toList $ directIntegrator2 i
#-}

{-# RULES
"morgans law 3" forall i. take 3 $ toList $ directIntegrator i = take 3 $ toList $ directIntegrator2 i
#-}

{-# RULES
"morgans law 2" forall i. take 2 $ toList $ directIntegrator i = take 2 $ toList $ directIntegrator2 i
#-}

{-# RULES
"morgans law 1" forall i. take 1 $ toList $ directIntegrator i = take 1 $ toList $ directIntegrator2 i
#-}

{-# RULES
"morgans law 0" forall i. take 0 $ toList $ directIntegrator i = take 0 $ toList $ directIntegrator2 i
#-}