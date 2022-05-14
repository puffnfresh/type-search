module Search.Structured
  ( Fingerprint (..),
    FingerprintError (..),
    FingerprintCost (..),
    FingerprintArityCost (..),
    FingerprintTermsCost (..),
    FingerprintRareCost (..),
    structuredMatchFingerprint,
    SignatureType (..),
    SignatureUnificationCost (..),
    Signature (..),
    SignatureCost (..),
    structuredMatchSignature,
    StructuredCosts (..),
    SignatureError (..),
    SearchLog (..),
    StructuredSearch,
    structuredSearch,
  )
where

import Control.Applicative (Applicative (liftA2))
import Control.Monad (zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (Writer, mapWriter, tell)
import Data.Equivalence.Monad (EquivT, classDesc, equate, runEquivT)
import Data.Foldable (fold)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import Data.Monoid (Sum (Sum, getSum))
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Data.Word (Word8)
import Search (Search (Search), SearchLimit)
import Streamly.Internal.Data.Stream.IsStream (hoist)
import Streamly.Prelude (SerialT)
import qualified Streamly.Prelude as Stream

data Fingerprint a
  = Fingerprint [a] Word8 Word8
  deriving (Eq, Ord, Show)

data FingerprintCost a
  = FingerprintCost FingerprintArityCost FingerprintTermsCost (FingerprintRareCost a)
  deriving (Eq, Ord, Show)

data FingerprintArityCost
  = FingerprintAritySame
  | FingerprintArityOneMore
  | FingerprintArityOneLess
  | FingerprintArityTwoLess
  deriving (Eq, Ord, Show)

data FingerprintTermsCost
  = FingerprintTermsSame
  | FingerprintTermsDifferent Word8
  deriving (Eq, Ord, Show)

data FingerprintRareCost a
  = FingerprintRareCost (Set a) (Set a)
  deriving (Eq, Ord, Show)

emptyEffect :: Monad m => m a -> SerialT m b
emptyEffect =
  Stream.concatMap Stream.fromList . Stream.fromEffect . fmap (const [])

structuredMatchFingerprint :: Ord a => Fingerprint a -> Fingerprint a -> SerialT (Writer [SearchLog a n]) (FingerprintCost a)
structuredMatchFingerprint (Fingerprint tr tt ta) candidate@(Fingerprint cr ct ca) =
  either
    (emptyEffect . tell . (: []) . SearchLogFingerprintError candidate)
    pure
    (liftA2 (\a t -> FingerprintCost a t rarity) arity terms)
  where
    arity =
      case (ta, ca, (fromIntegral ca - fromIntegral ta) :: Int) of
        (0, 0, _) ->
          Right FingerprintAritySame
        (0, _, _) ->
          Left FingerprintArityZero
        (_, 0, _) ->
          Left FingerprintArityZero
        (_, _, 0) ->
          Right FingerprintAritySame
        (_, _, -1) ->
          Right FingerprintArityOneMore
        (_, _, 1) ->
          Right FingerprintArityOneLess
        (_, _, 2) ->
          Right FingerprintArityTwoLess
        (_, _, _) ->
          Left FingerprintArityTooDifferent

    terms =
      case fromIntegral ct - fromIntegral tt of
        0 ->
          Right FingerprintTermsSame
        n
          | abs n > 20 ->
            Left FingerprintTermsTooDifferent
        n ->
          Right (FingerprintTermsDifferent n)

    tr' =
      Set.fromList tr
    cr' =
      Set.fromList cr

    rarity =
      FingerprintRareCost (tr' \\ cr') (cr' \\ tr')

-- Haskell:
-- (a -> b) -> [a] -> [b]

-- Scala:
-- [A, B](l: List[A], f: A => B): List[B]

-- TypeScript:
-- <A, B>(l: A[], f: (a: A) => B): B[]

-- Java:
-- List<B> map<A, B>(l: List<A>, f: Function<A, B>)

data SignatureType n
  = SignatureTypeConstructor n [SignatureType n]
  | SignatureTypeVariable n [SignatureType n]
  | SignatureTypeFunction [SignatureType n] (SignatureType n)
  deriving (Eq, Ord, Show)

instance Foldable SignatureType where
  foldMap f (SignatureTypeConstructor n ns) =
    f n <> (foldMap . foldMap) f ns
  foldMap f (SignatureTypeVariable n ns) =
    f n <> (foldMap . foldMap) f ns
  foldMap f (SignatureTypeFunction ns n) =
    (foldMap . foldMap) f ns <> foldMap f n

instance Functor SignatureType where
  fmap f (SignatureTypeConstructor n ns) =
    SignatureTypeConstructor (f n) ((fmap . fmap) f ns)
  fmap f (SignatureTypeVariable n ns) =
    SignatureTypeVariable (f n) ((fmap . fmap) f ns)
  fmap f (SignatureTypeFunction ns n) =
    SignatureTypeFunction ((fmap . fmap) f ns) (fmap f n)

instance Traversable SignatureType where
  traverse f (SignatureTypeConstructor n ns) =
    liftA2 SignatureTypeConstructor (f n) ((traverse . traverse) f ns)
  traverse f (SignatureTypeVariable n ns) =
    liftA2 SignatureTypeVariable (f n) ((traverse . traverse) f ns)
  traverse f (SignatureTypeFunction ns n) =
    liftA2 SignatureTypeFunction ((traverse . traverse) f ns) (traverse f n)

newtype SignatureUnificationCost
  = SignatureUnificationCost Word8
  deriving (Eq, Ord, Show)

newtype Signature n
  = Signature (NonEmpty (SignatureType n))
  deriving (Eq, Ord, Show)

instance Foldable Signature where
  foldMap f (Signature ts) =
    (foldMap . foldMap) f ts

instance Functor Signature where
  fmap f (Signature ts) =
    Signature ((fmap . fmap) f ts)

instance Traversable Signature where
  traverse f (Signature ts) =
    Signature <$> (traverse . traverse) f ts

newtype SignatureCost
  = SignatureCost SignatureUnificationCost
  deriving (Eq, Ord, Show)

structuredMatchSignature :: Ord n => Signature n -> Signature n -> SerialT (Writer [SearchLog a n]) SignatureCost
structuredMatchSignature (Signature queryTypes) signature@(Signature signatureTypes) = do
  equivalenceCost <- hoist (mapWriter ((fmap . fmap) (SearchLogSignatureError signature))) (unifyTypes (signatureFunction queryTypes) (signatureFunction signatureTypes))
  -- TODO: Add support for contexts
  pure (SignatureCost (SignatureUnificationCost equivalenceCost))

data TypeDescription n
  = TypeConstructor n
  | TypeVariable n
  deriving (Eq, Ord, Show)

unifyTypes :: (Ord n) => SignatureType n -> SignatureType n -> SerialT (Writer [SignatureError]) Word8
unifyTypes x y =
  getSum <$> runEquivT id preferConstructor (go x y)
  where
    go (SignatureTypeConstructor a as) (SignatureTypeConstructor b bs) =
      unifyArguments (unifyName (TypeConstructor a) (TypeConstructor b)) as bs
    go (SignatureTypeConstructor a as) (SignatureTypeVariable b bs) =
      unifyArguments (unifyName (TypeConstructor a) (TypeVariable b)) as bs
    go (SignatureTypeVariable a as) (SignatureTypeVariable b bs) =
      unifyArguments (unifyName (TypeVariable a) (TypeVariable b)) as bs
    go (SignatureTypeFunction as a) (SignatureTypeFunction bs b) =
      unifyArguments (go a b) as bs
    go _ _ =
      notUnified
    unifyArguments initalWork as bs = do
      work <- initalWork
      if length as == length bs
        then fold . (work :) <$> zipWithM go as bs
        else notUnified
    unifyName a b = do
      a' <- classDesc a
      b' <- classDesc b
      if a' /= b'
        then do
          case (a', b') of
            (TypeConstructor _, TypeConstructor _) ->
              notUnified
            _ ->
              pure ()
          equate a' b'
          pure (Sum 1)
        else pure (Sum 0)
    preferConstructor (TypeConstructor n) _ =
      TypeConstructor n
    preferConstructor _ (TypeConstructor n) =
      TypeConstructor n
    preferConstructor a _ =
      a

notUnified :: EquivT s (TypeDescription n) (TypeDescription n) (SerialT (Writer [SignatureError])) a
notUnified =
  lift (emptyEffect (tell [SignatureNotUnified]))

signatureFunction :: NonEmpty (SignatureType n) -> SignatureType n
signatureFunction ts =
  SignatureTypeFunction (NEL.init ts) (NEL.last ts)

data StructuredCosts a
  = StructuredCosts (FingerprintCost a) SignatureCost
  deriving (Eq, Ord, Show)

data FingerprintError
  = FingerprintArityZero
  | FingerprintArityTooDifferent
  | FingerprintTermsTooDifferent
  deriving (Eq, Ord, Show)

data SignatureError
  = SignatureNotUnified
  deriving (Eq, Ord, Show)

data SearchLog a n
  = SearchLogFingerprintError (Fingerprint a) FingerprintError
  | SearchLogSignatureError (Signature n) SignatureError
  deriving (Eq, Ord, Show)

type StructuredSearch a n =
  Search (Writer [SearchLog a n]) (Fingerprint a) (FingerprintCost a) (Signature n) SignatureCost (StructuredCosts a)

structuredSearch :: (Ord a, Ord n) => SearchLimit -> (FingerprintCost a -> Int) -> (SignatureCost -> Int) -> StructuredSearch a n
structuredSearch searchLimit f g =
  Search searchLimit structuredMatchFingerprint' structuredMatchSignature (\a b -> (f a + g b, StructuredCosts a b))
  where
    structuredMatchFingerprint' =
      (fmap . fmap) (\a -> (f a, a)) . structuredMatchFingerprint
