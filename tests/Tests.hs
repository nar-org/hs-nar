{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Control.Applicative ((<$>))
import Control.Monad

import Archive.Nar.UTF8

newtype UtfTest = UtfTest String
    deriving (Show,Eq)

instance Arbitrary UtfTest where
    arbitrary = UtfTest <$> (choose (0,38) >>= flip replicateM arbitraryChar)
      where arbitraryChar = frequency
                                [ (1, toEnum <$> choose (0, 127))
                                , (2, toEnum <$> choose (128, 0x10ffff))
                                ]

knownChars =
    [ ("$", ([0x0024], "$"))
    , ("¢", ([0x00A2], "\xc2\xa2"))
    , ("€", ([0x20AC], "\xe2\x82\xac"))
    ]

knownCases = concatMap toTC knownChars
  where toTC (s, (uniPoint, encoded)) =
            [ testCase ("code point " ++ s) (uniPoint @=? map fromEnum s)
            , testCase ("encoding " ++ s) (encoded @=? utf8Encode s)
            , testCase ("decoding " ++ s) (s @=? utf8Decode encoded)
            ]

main :: IO ()
main = defaultMain $ testGroup "NAR"
    [ testGroup "utf8"
        ( knownCases ++
        [ testProperty "decode . encode == id" (\(UtfTest s) -> utf8Decode (utf8Encode s) == s)
        ])
    ]
