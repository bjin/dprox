-- SPDX-License-Identifier: BSD-3-Clause
--
-- Copyright (C) 2019 Bin Jin. All Rights Reserved.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DomainRoute
  ( DomainRoute
  , getDomainRouteByPrefix
  , getDomainRouteExact
  , newDomainRoute
  ) where

import Data.ByteString.Char8 qualified as BS
import Data.Trie             qualified as T
import Data.Trie.Convenience qualified as T
import Network.DNS           qualified as DNS

newtype DomainRoute a = DomainRoute (T.Trie a)
    deriving (Eq, Show, Functor, Foldable)

normalize :: DNS.Domain -> BS.ByteString
normalize d
    | BS.null d = BS.singleton '.'
    | otherwise = BS.reverse (BS.cons '.' (DNS.normalize d))

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
