{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Writer (runWriter)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Set (fromList)
import Hedgehog (Gen, Group (..), Property, checkParallel, forAll, property, withTests, (===))
import Hedgehog.Gen (element, unicodeAll)
import Hedgehog.Main (defaultMain)
import Search (Search (Search), SearchLimit (SearchLimit), runSearch)
import Search.Hoogle (hoogle, hoogleFingerprint)
import qualified Search.Structured as S
import qualified Streamly.Prelude as Stream

main :: IO ()
main =
  defaultMain
    [ checkParallel
        ( Group
            "Search.Hoogle"
            [ ("example Hoogle search", propertyExampleHoogle)
            ]
        )
    ]

propertyExampleHoogle :: Property
propertyExampleHoogle =
  withTests 1 . property $ do
    let example =
          S.Signature (S.SignatureTypeConstructor "Int" [] :| [S.SignatureTypeConstructor "String" []])
        signatures =
          [ example,
            S.Signature (S.SignatureTypeVariable "a" [] :| [S.SignatureTypeVariable "b" []]),
            S.Signature (S.SignatureTypeVariable "b" [] :| [S.SignatureTypeVariable "b" []]),
            S.Signature (S.SignatureTypeVariable "a" [] :| [S.SignatureTypeConstructor "String" []]),
            S.Signature (S.SignatureTypeConstructor "Int" [] :| [S.SignatureTypeVariable "b" []])
          ]
        database =
          (\s -> (s, fingerprint s)) <$> signatures
        fingerprint =
          hoogleFingerprint popularity
        popularity _ =
          maxBound
    runWriter (Stream.toList (runSearch (hoogle popularity) database (fingerprint example) example))
      === ( [ ( S.Signature
                  ( S.SignatureTypeConstructor "Int" []
                      :| [S.SignatureTypeConstructor "String" []]
                  ),
                S.StructuredCosts
                  ( S.FingerprintCost
                      S.FingerprintAritySame
                      S.FingerprintTermsSame
                      (S.FingerprintRareCost (fromList []) (fromList []))
                  )
                  (S.SignatureCost (S.SignatureUnificationCost 0))
              ),
              ( S.Signature
                  ( S.SignatureTypeVariable "a" []
                      :| [S.SignatureTypeConstructor "String" []]
                  ),
                S.StructuredCosts
                  ( S.FingerprintCost
                      S.FingerprintAritySame
                      S.FingerprintTermsSame
                      (S.FingerprintRareCost (fromList ["Int"]) (fromList ["a"]))
                  )
                  (S.SignatureCost (S.SignatureUnificationCost 1))
              ),
              ( S.Signature
                  ( S.SignatureTypeConstructor "Int" []
                      :| [S.SignatureTypeVariable "b" []]
                  ),
                S.StructuredCosts
                  ( S.FingerprintCost
                      S.FingerprintAritySame
                      S.FingerprintTermsSame
                      (S.FingerprintRareCost (fromList ["String"]) (fromList ["b"]))
                  )
                  (S.SignatureCost (S.SignatureUnificationCost 1))
              ),
              ( S.Signature
                  (S.SignatureTypeVariable "a" [] :| [S.SignatureTypeVariable "b" []]),
                S.StructuredCosts
                  ( S.FingerprintCost
                      S.FingerprintAritySame
                      S.FingerprintTermsSame
                      ( S.FingerprintRareCost
                          (fromList ["Int", "String"])
                          (fromList ["a", "b"])
                      )
                  )
                  (S.SignatureCost (S.SignatureUnificationCost 2))
              )
            ],
            [ S.SearchLogSignatureError
                ( S.Signature
                    (S.SignatureTypeVariable "b" [] :| [S.SignatureTypeVariable "b" []])
                )
                S.SignatureNotUnified
            ]
          )
