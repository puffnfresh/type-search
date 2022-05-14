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
import Streamly.Internal.Data.Stream.IsStream (concatPairsWith)
import Streamly.Prelude (IsStream, SerialT)
import qualified Streamly.Prelude as Stream

newtype SearchLimit
  = SearchLimit Word8

data Search m a a' b b' c
  = Search
      SearchLimit
      (a -> a -> SerialT m (Int, a'))
      (b -> b -> SerialT m b')
      (a' -> b' -> (Int, c))

takeLimit :: Monad m => SearchLimit -> SerialT m a -> SerialT m a
takeLimit (SearchLimit a) =
  Stream.take (fromIntegral a)

searchLimit :: Lens' (Search m a a' b b' c) SearchLimit
searchLimit f (Search a b c d) =
  (\a' -> Search a' b c d) <$> f a

searchMatchFingerprint :: Lens' (Search m a a' b b' c) (a -> a -> SerialT m (Int, a'))
searchMatchFingerprint f (Search a b c d) =
  (\b' -> Search a b' c d) <$> f b

searchMatchSignature :: Lens' (Search m a a' b b' c) (b -> b -> SerialT m b')
searchMatchSignature f (Search a b c d) =
  (\c' -> Search a b c' d) <$> f c

searchCombineCosts :: Lens' (Search m a a' b b' c) (a' -> b' -> (Int, c))
searchCombineCosts f (Search a b c d) =
  Search a b c <$> f d

sortOn' :: (IsStream t, Monad m) => (b -> b -> Ordering) -> t m b -> t m b
sortOn' f =
  concatPairsWith (Stream.mergeBy f) Stream.fromPure

applyCosts :: (Monad n, Ord z) => Search m a a' b b' c -> SerialT n (x, (z, y)) -> SerialT n (x, y)
applyCosts search =
  fmap (\(x, (_, y)) -> (x, y)) . takeLimit (search ^. searchLimit) . sortOn' (comparing (fst . snd))

-- | Find some candidates using a fast search (e.g. fingerprints) then look
-- | into each candidate using an expensive matching algorithm (e.g.
-- | unification) - both calculate a "cost" for each match which is used to
-- | sort results.
runSearch :: Monad m => Search m a a' b b' c -> [(b, a)] -> a -> b -> SerialT m (b, c)
runSearch search fingerprints fingerprint query =
  applyCosts search $ do
    (candidate, fingerprintCost) <- applyCosts search bestByFingerprint
    signatureCost <- (search ^. searchMatchSignature) query candidate
    pure (candidate, (search ^. searchCombineCosts) fingerprintCost signatureCost)
  where
    bestByFingerprint = do
      (b, fingerprint') <- Stream.fromList fingerprints
      fingerprintCost <- (search ^. searchMatchFingerprint) fingerprint fingerprint'
      pure (b, fingerprintCost)
