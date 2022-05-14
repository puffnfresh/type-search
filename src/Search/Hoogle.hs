module Search.Hoogle
  ( Popularity (..),
    hoogleFingerprint,
    hoogle,
  )
where

import Data.Foldable (toList)
import Data.List (sortOn)
import Data.Monoid (Sum (Sum, getSum))
import Data.Word (Word8)
import Search (SearchLimit (SearchLimit))
import qualified Search.Structured as S

-- minBound meaning least popular
-- maxBound meaning most popular
newtype Popularity
  = Popularity Word8
  deriving (Eq, Ord, Show, Bounded)

scalePopularity :: Popularity -> Double
scalePopularity (Popularity n) =
  fromIntegral n / fromIntegral (maxBound :: Word8)

fingerprintCost :: (n -> Popularity) -> S.FingerprintCost n -> Int
fingerprintCost popularity (S.FingerprintCost ac tc rc) =
  arityCost ac + termsCost tc + rareCost popularity rc

arityCost :: S.FingerprintArityCost -> Int
arityCost S.FingerprintAritySame =
  0
arityCost S.FingerprintArityOneMore =
  1000
arityCost S.FingerprintArityOneLess =
  300
arityCost S.FingerprintArityTwoLess =
  900

termsCost :: S.FingerprintTermsCost -> Int
termsCost S.FingerprintTermsSame =
  0
termsCost (S.FingerprintTermsDifferent w) =
  fromIntegral w

rareCost :: (n -> Popularity) -> S.FingerprintRareCost n -> Int
rareCost popularity (S.FingerprintRareCost a b) =
  getSum (foldMap (f 5000 400) a <> foldMap (f 1000 50) b)
  where
    f common rare s =
      let p =
            scalePopularity (popularity s)
          common' =
            p * common
          rare' =
            (1 - p) * rare
       in Sum (floor (common' + rare'))

signatureCost :: S.SignatureCost -> Int
signatureCost (S.SignatureCost uc) =
  signatureUnificationCost uc

signatureUnificationCost :: S.SignatureUnificationCost -> Int
signatureUnificationCost (S.SignatureUnificationCost uc) =
  fromIntegral uc

hoogleFingerprint :: (a -> Popularity) -> S.Signature a -> S.Fingerprint a
hoogleFingerprint f signature@(S.Signature ts) =
  S.Fingerprint rare (fromIntegral (length terms)) (fromIntegral arity)
  where
    rare =
      take 3 (sortOn f terms)
    terms =
      toList signature
    arity =
      length ts - 1

hoogle ::
  (Ord a, Ord n) =>
  (a -> Popularity) ->
  S.StructuredSearch a n
hoogle popularity =
  S.structuredSearch (SearchLimit 100) (fingerprintCost popularity) signatureCost
