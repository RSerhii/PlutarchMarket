{-# LANGUAGE CPP #-}

module Main (main) where

import Test.Tasty
import Prelude (IO)
import Spec (spec)

main :: IO ()
main = defaultMain spec
