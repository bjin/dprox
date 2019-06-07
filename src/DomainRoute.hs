-- SPDX-License-Identifier: BSD-3-Clause
--
-- Copyright (C) 2019 Bin Jin. All Rights Reserved.
{-# LANGUAGE RecordWildCards #-}
module DomainRoute
( DomainRoute
, newDomainRoute
, getDomainRouteExact
, getDomainRouteByPrefix
) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Short as SBS
import           Data.Function         (on)
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HashMap
import           Data.List             (groupBy, sortOn, span)
import qualified Network.DNS           as DNS

type DomainComp = SBS.ShortByteString

data DomainRoute a = DomainRoute
    { currentNode :: Maybe a
    , children    :: Maybe (HashMap DomainComp (DomainRoute a))
    } deriving (Eq, Show)

instance Functor DomainRoute where
    fmap f DomainRoute{..} = DomainRoute (fmap f currentNode) ((fmap.fmap.fmap) f children)

instance Foldable DomainRoute where
    foldMap f DomainRoute{..} = foldMap f currentNode `mappend` (foldMap.foldMap.foldMap) f children

newDomainRoute :: (a -> a -> a) -> [(DNS.Domain, a)] -> DomainRoute a
newDomainRoute merge servers = go sortedServers
  where
    fmapFst f (x, y) = (f x, y)
    sortedServers = sortOn fst $ map (fmapFst getDomainComponents) servers

    go [] = DomainRoute Nothing Nothing
    go lst = DomainRoute node childs
      where
        (prefix, suffix) = span (null . fst) lst
        grouped = groupBy ((==) `on` (head.fst)) suffix

        node = if null prefix then Nothing else Just (foldr1 merge (map snd prefix))
        subtrees = [ (comp, go subtree)
                   | child <- grouped
                   , let comp = head (fst (head child))
                   , let subtree = map (fmapFst tail) child
                   ]
        childs = if null subtrees then Nothing else Just (HashMap.fromList subtrees)

queryDomainRoute :: Bool -> DomainRoute a -> DNS.Domain -> Maybe a
queryDomainRoute allowPrefix dr domain = go dr comps
  where
    comps = getDomainComponents domain

    placePrefix _ b       | not allowPrefix = b
    placePrefix a Nothing = a
    placePrefix _ b       = b

    go DomainRoute{..} [] = currentNode
    go (DomainRoute node Nothing) _ = placePrefix node Nothing
    go (DomainRoute node (Just child)) (x:xs) = placePrefix node $ case HashMap.lookup x child of
        Nothing      -> Nothing
        Just subtree -> go subtree xs

getDomainRouteExact :: DomainRoute a -> DNS.Domain -> Maybe a
getDomainRouteExact = queryDomainRoute False

getDomainRouteByPrefix :: DomainRoute a -> DNS.Domain -> Maybe a
getDomainRouteByPrefix = queryDomainRoute True

getDomainComponents :: DNS.Domain -> [DomainComp]
getDomainComponents domain = reverse $ filter (not . SBS.null) comps
  where
    comps = map SBS.toShort $ BS8.split '.' (DNS.normalizeCase domain)
