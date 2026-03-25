{-# LANGUAGE TypeFamilies #-}

module Search
  ( SearchLimit (..),
    Search (..),
    searchLimit,
    searchMatchFingerprint,
    searchMatchSignature,
    runSearch,
  )
where

import Control.Lens (Lens', (^.))
import Data.Ord (comparing)
import Data.Word (Word8)
import Streamly.Data.Stream (Stream, fromList, fromPure, toList)
import Streamly.Data.StreamK (fromStream, toStream)
import Streamly.Internal.Data.Stream (Nested (Nested, unNested))
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.StreamK as StreamK

newtype SearchLimit
  = SearchLimit Word8

data Search m a a' b b' c
  = Search
      SearchLimit
      (a -> a -> Stream m (Int, a'))
      (b -> b -> Stream m b')
      (a' -> b' -> (Int, c))

takeLimit :: Monad m => SearchLimit -> Stream m a -> Stream m a
takeLimit (SearchLimit a) =
  Stream.take (fromIntegral a)

searchLimit :: Lens' (Search m a a' b b' c) SearchLimit
searchLimit f (Search a b c d) =
  (\a' -> Search a' b c d) <$> f a

searchMatchFingerprint :: Lens' (Search m a a' b b' c) (a -> a -> Stream m (Int, a'))
searchMatchFingerprint f (Search a b c d) =
  (\b' -> Search a b' c d) <$> f b

searchMatchSignature :: Lens' (Search m a a' b b' c) (b -> b -> Stream m b')
searchMatchSignature f (Search a b c d) =
  (\c' -> Search a b c' d) <$> f c

searchCombineCosts :: Lens' (Search m a a' b b' c) (a' -> b' -> (Int, c))
searchCombineCosts f (Search a b c d) =
  Search a b c <$> f d

applyCosts :: (Monad n, Ord z) => Search m a a' b b' c -> Stream n (x, (z, y)) -> Stream n (x, y)
applyCosts search =
  fmap (\(x, (_, y)) -> (x, y)) . takeLimit (search ^. searchLimit) . toStream . StreamK.sortBy (comparing (fst . snd)) . fromStream

-- | Find some candidates using a fast search (e.g. fingerprints) then look
-- | into each candidate using an expensive matching algorithm (e.g.
-- | unification) - both calculate a "cost" for each match which is used to
-- | sort results.
runSearch :: Monad m => Search m a a' b b' c -> [(b, a)] -> a -> b -> Stream m (b, c)
runSearch search fingerprints fingerprint query =
  applyCosts search $ unNested $ do
    (candidate, fingerprintCost) <- Nested (applyCosts search bestByFingerprint)
    signatureCost <- Nested ((search ^. searchMatchSignature) query candidate)
    pure (candidate, (search ^. searchCombineCosts) fingerprintCost signatureCost)
  where
    bestByFingerprint = unNested $ do
      (b, fingerprint') <- Nested (fromList fingerprints)
      fingerprintCost <- Nested ((search ^. searchMatchFingerprint) fingerprint fingerprint')
      pure (b, fingerprintCost)
