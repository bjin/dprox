-- SPDX-License-Identifier: BSD-3-Clause
--
-- Copyright (C) 2019 Bin Jin. All Rights Reserved.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DomainRoute
( DomainRoute
, newDomainRoute
, getDomainRouteExact
, getDomainRouteByPrefix
) where

import qualified Data.ByteString.Char8 as BS
import           Data.Char             (toLower)
import qualified Data.Trie             as T
import qualified Data.Trie.Convenience as T
import qualified Network.DNS           as DNS

newtype DomainRoute a = DomainRoute (T.Trie a)
    deriving (Eq, Show, Functor, Foldable)

normalize :: DNS.Domain -> BS.ByteString
normalize d
    | BS.null d = BS.empty
    | otherwise = BS.reverse (BS.cons '.' (BS.map toLower d))

newDomainRoute :: (a -> a -> a) -> [(DNS.Domain, a)] -> DomainRoute a
newDomainRoute merge ds = DomainRoute (T.fromListWithL' merge ds')
  where
    ds' =  [(normalize domain, record) | (domain, record) <- ds]

getDomainRouteExact :: DomainRoute a -> DNS.Domain -> Maybe a
getDomainRouteExact (DomainRoute trie) d = T.lookup (normalize d) trie

getDomainRouteByPrefix :: DomainRoute a -> DNS.Domain -> Maybe a
getDomainRouteByPrefix (DomainRoute trie) d = case T.match trie (normalize d) of
    Nothing          -> Nothing
    Just (_, res, _) -> Just res
