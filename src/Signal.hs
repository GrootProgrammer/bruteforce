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
directIntegrator = mealy (\(a1, a2) b -> ((b, a1), a1 + a2)) (0,0)

directIntegrator2 :: Signal Integer -> Signal Integer
directIntegrator2 i = foldl addS (sRepeat 0) (take 4 $ iterate (register 0) i)

{-# RULES
"morgans law" forall i. take 5 $ toList $ register (0 :: Integer) (directIntegrator2 i) =  take 5 $ toList $ directIntegrator2 (register (0 :: Integer) i)
#-}