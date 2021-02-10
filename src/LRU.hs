-- SPDX-License-Identifier: BSD-3-Clause
--
-- Copyright (C) 2021 Bin Jin. All Rights Reserved.
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module LRU
( LRUCache
, newCache
, lookupCache
, updateCache
, purgeCache
) where

import qualified Data.HashPSQ           as PQ
import           Data.Hashable          (Hashable)
import           Data.IORef             (IORef, atomicModifyIORef', newIORef,
                                         readIORef)
import           Data.Int               (Int64)
import           Data.Time.Clock.System (SystemTime (..), getSystemTime)
import           Network.DNS            (TTL)

type Time = Int64

getTime :: IO Time
getTime = systemSeconds <$> getSystemTime

data LRUQueue k v = LRUQueue
    { queueSize :: !Int
    , queue     :: !(PQ.HashPSQ k Time v)
    } deriving (Show)

newQueue :: LRUQueue k v
newQueue = LRUQueue 0 PQ.empty

lookupQueue :: (Ord k, Hashable k) => k -> LRUQueue k v -> Maybe (Time, v)
lookupQueue k = PQ.lookup k . queue

updateQueue :: (Ord k, Hashable k) => Int -> Time -> k -> v -> LRUQueue k v -> LRUQueue k v
updateQueue cacheSize expireTime k v LRUQueue{..} = case PQ.insertView k expireTime v queue of
    (Just _, newQ)  -> LRUQueue queueSize newQ
    (Nothing, newQ) -> if queueSize >= cacheSize
                       then LRUQueue queueSize (PQ.deleteMin newQ)
                       else LRUQueue (queueSize+1) newQ

purgeQueue :: (Ord k, Hashable k) => Time -> LRUQueue k v -> LRUQueue k v
purgeQueue _ q@(LRUQueue 0 _) = q
purgeQueue now LRUQueue{..} = LRUQueue (queueSize - length expired) newQ
  where
    (expired, newQ) = PQ.atMostView now queue

data LRUCache k v = LRUCache
    { cacheSize  :: !Int
    , timeToLive :: !TTL
    , cacheRef   :: IORef (LRUQueue k v)
    }

newCache :: Int -> TTL -> IO (LRUCache k v)
newCache sz ttl = LRUCache sz ttl <$> newIORef newQueue

lookupCache :: (Ord k, Hashable k) => k -> LRUCache k v -> IO (Maybe (TTL, v))
lookupCache k LRUCache{..} = do
    now <- getTime
    res <- lookupQueue k <$> readIORef cacheRef
    return $ case res of
        Nothing              -> Nothing
        Just (expireTime, v) -> if expireTime <= now
                                then Nothing
                                else Just (fromIntegral (expireTime - now), v)

updateCache :: (Ord k, Hashable k) => k -> v -> LRUCache k v -> IO ()
updateCache k v LRUCache{..} = do
    now <- getTime
    atomicModifyIORef' cacheRef ((,()) . updateQueue cacheSize (now+fromIntegral timeToLive) k v)

purgeCache :: (Ord k, Hashable k) => LRUCache k v -> IO ()
purgeCache LRUCache{..} = do
    now <- getTime
    atomicModifyIORef' cacheRef ((,()) . purgeQueue now)
