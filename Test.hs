{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad         (forM_)
import qualified Data.ByteString.Char8 as BS8
import           Data.Char             (toUpper)
import           Test.Hspec

import           DomainRoute

main :: IO ()
main = hspec $ do
    let query domain = BS8.count '.' domain
        build ds = [(domain, query domain) | domain <- ds]
        domains = ["cn", "chn.net", "red.com", "black.com", "www.red.com", "a.b.c.d.red.com"]
        dr1 = newDomainRoute const (build domains)

    describe "newDomainRoute" $ do
        let dr2 = newDomainRoute const (build domains ++ build domains)
            dr3 = newDomainRoute (flip const) (map (fmap (+1)) (build domains) ++ build domains)
            dr4 = newDomainRoute const (build (map (BS8.map toUpper) domains))
        it "handles duplicated domains" $
          dr1 `shouldBe` dr2

        it "handles duplicated domains with correct merge" $
          dr1 `shouldBe` dr3

        it "handles domains without case sensibility" $
          dr1 `shouldBe` dr4

    describe "getDomainRouteExact" $ do
        it "handles positive samples" $ forM_ domains $ \domain ->
            getDomainRouteExact dr1 domain `shouldBe` Just (query domain)

        it "handles negative samples" $ forM_ ["", "net", "com", "www.black.com"] $ \domain ->
            getDomainRouteExact dr1 domain `shouldBe` Nothing

    describe "getDomainRouteByPrefix" $ do
        it "handles positive samples" $ forM_ domains $ \domain ->
            getDomainRouteByPrefix dr1 domain `shouldBe` Just (query domain)

        it "handles positive samples with some new prefix" $ forM_ domains $ \domain ->
            getDomainRouteByPrefix dr1 ("prefix." `BS8.append` domain) `shouldBe` Just (query domain)

        it "handles negative samples" $ forM_ ["", "net", "com", "chn2.net"] $ \domain ->
            getDomainRouteByPrefix dr1 domain `shouldBe` Nothing

        it "handles the longest match" $ forM_ [("c.d.red.com", 1), ("www.a.b.c.d.red.com", 5)] $ \(domain, ans) ->
            getDomainRouteByPrefix dr1 domain `shouldBe` Just ans
