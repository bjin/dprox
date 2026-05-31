-- SPDX-License-Identifier: BSD-3-Clause
--
-- Copyright (C) 2019 Bin Jin. All Rights Reserved.
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad         (forM_)
import Data.ByteString.Char8 qualified as BS8
import Data.Char             (toUpper)
import Test.Hspec

import DomainRoute
import Unix

main :: IO ()
main = hspec $ do
    let query = BS8.count '.'
        build ds = [(domain, query domain) | domain <- ds]
        domains = ["cn", "chn.net", "red.com", "black.com", "www.red.com", "a.b.c.d.red.com"]
        dr1 = newDomainRoute const (build domains)
        dr0 = newDomainRoute const (build ("" : domains))

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

        it "handles root domain fallbacks" $ forM_ ["", "net", "com", "chn2.net"] $ \domain ->
            getDomainRouteByPrefix dr0 domain `shouldBe` Just 0

        it "handles the longest match" $ forM_ [("c.d.red.com", query "red.com"), ("www.a.b.c.d.red.com", query "a.b.c.d.red.com")] $ \(domain, ans) ->
            getDomainRouteByPrefix dr1 domain `shouldBe` Just ans

    describe "planPrivilegeDrop" $ do
        let alice = ResolvedUser "alice" 1000 10
            primary = ResolvedGroup "primary" 10 ["alice"]
            target = ResolvedGroup "target" 20 []
            extra = ResolvedGroup "extra" 30 ["alice"]
            unrelated = ResolvedGroup "unrelated" 40 ["bob"]

        it "uses the requested group when both user and group are configured" $
            planPrivilegeDrop (Just alice) (Just target) [primary, extra, unrelated]
                `shouldBe` PrivilegeDropPlan (Just "alice") (Just 1000) (Just "target") (Just 20) [20, 10, 30]

        it "falls back to the user's primary group when no group is configured" $
            planPrivilegeDrop (Just alice) Nothing [primary, extra, unrelated]
                `shouldBe` PrivilegeDropPlan (Just "alice") (Just 1000) Nothing (Just 10) [10, 30]

        it "does not assign supplementary groups for group-only drops" $
            planPrivilegeDrop Nothing (Just target) [primary, extra, unrelated]
                `shouldBe` PrivilegeDropPlan Nothing Nothing (Just "target") (Just 20) []

