{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Aeson qualified as Aeson
import Data.Aeson.QQ.Simple as Aeson
import Data.Text (Text)
import JSONMask

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" testCases

data TestCase =
  TestCase
    { expected :: Aeson.Value
    , object   :: Aeson.Value
    , fields   :: Text
    }

mkTest :: String -> TestCase -> TestTree
mkTest name tc = testCase name $
  (`maskValue` object tc) <$> parseMask (fields tc) @?= Right (expected tc)

testCases :: [TestTree]
testCases =
  [ nullCase
  , nonField
  , nullField
  , nonFieldArray
  , noMaskIdentity
  , singleTopLevelField
  , multipleFields
  , nestedField
  , nestedFieldArray
  , nestedMultiple
  , wildcardAll
  , wildcardNested
  , wildcardNestedLeaf
  , singleSubSelectorArray
  , subSelectNestedWildcard
  , subSelectSiblingNestedArrayField
  , wildcardSubSelect
  , escapedField
  , escapedWildcard
  ]

nullCase :: TestTree
nullCase = mkTest "masking null" $ TestCase Aeson.Null Aeson.Null "a"

nonField :: TestTree
nonField = mkTest "non-existent field" $ TestCase e o "a"
  where
    e = [aesonQQ|{}|]
    o = [aesonQQ|{"b":1}|]

nullField :: TestTree
nullField = mkTest "null field" $ TestCase e o "a"
  where
    e = [aesonQQ|{"a":null}|]
    o = [aesonQQ|{"a":null,"b":1}|]

nonFieldArray :: TestTree
nonFieldArray = mkTest "non-existent field in array" $ TestCase e o "a"
  where
    e = [aesonQQ|[{}]|]
    o = [aesonQQ|[{"b":1}]|]

noMaskIdentity :: TestTree
noMaskIdentity = mkTest "no mask" $ TestCase e o "a"
  where
    e = [aesonQQ|{"a":1}|]
    o = [aesonQQ|{"a":1}|]

singleTopLevelField :: TestTree
singleTopLevelField = mkTest "single top-level field" $ TestCase e o "a"
  where
    e = [aesonQQ|{"a":1}|]
    o = [aesonQQ|{"a":1,"b":1}|]

multipleFields :: TestTree
multipleFields = mkTest "multiple top-level fields" $ TestCase e o "a,b"
  where
    e = [aesonQQ|{"a":1,"b":1}|]
    o = [aesonQQ|{"a":1,"b":1,"c":1}|]

nestedField :: TestTree
nestedField = mkTest "nested field" $ TestCase e o "obj/s"
  where
    e = [aesonQQ|{"obj":{"s":1}}|]
    o = [aesonQQ|{"obj":{"s":1,"t":2},"b":1}|]

nestedFieldArray :: TestTree
nestedFieldArray = mkTest "nested field in array" $ TestCase e o "arr/s"
  where
    e = [aesonQQ|{"arr":[{"s":1},{"s":2}]}|]
    o = [aesonQQ|{"arr":[{"s":1,"t":2},{"s":2,"t":3}],"b":1}|]

nestedMultiple :: TestTree
nestedMultiple = mkTest "nested select with top-level sibling" $ TestCase e o "a/s/g,b"
  where
    e = [aesonQQ|{"a":{"s":{"g":1}},"b":1}|]
    o = [aesonQQ|{"a":{"s":{"g":1,"z":1}},"t":2,"b":1}|]

wildcardAll :: TestTree
wildcardAll = mkTest "top-level wildcard" $ TestCase e o "*"
  where
    e = [aesonQQ|{"a":2,"b":null,"c":0,"d":3}|]
    o = [aesonQQ|{"a":2,"b":null,"c":0,"d":3}|]

wildcardNested :: TestTree
wildcardNested = mkTest "nested under wildcard" $ TestCase e o "a/*/g"
  where
    e = [aesonQQ|{"a":{"s":{"g":3},"t":{"g":4},"u":{}}}|]
    o = [aesonQQ|{"a":{"s":{"g":3},"t":{"g":4},"u":{"z":1}},"b":1}|]

wildcardNestedLeaf :: TestTree
wildcardNestedLeaf = mkTest "nested wildcard" $ TestCase e o "a/*"
  where
    e = [aesonQQ|{"a":{"s":{"g":3},"t":{"g":4},"u":{"z":1}}}|]
    o = [aesonQQ|{"a":{"s":{"g":3},"t":{"g":4},"u":{"z":1}},"b":1}|]

singleSubSelectorArray :: TestTree
singleSubSelectorArray = mkTest "single sub-select on array" $ TestCase e o "a(g)"
  where
    e = [aesonQQ|{"a":[{"g":1},{"g":2}]}|]
    o = [aesonQQ|{"a":[{"g":1,"d":2},{"g":2,"d":3}]}|]

subSelectNestedWildcard :: TestTree
subSelectNestedWildcard = mkTest "sub-select nested field in array" $ TestCase e o "b(d/*/z)"
  where
    e = [aesonQQ|{"b":[{"d":{"g":{"z":22}}}]}|]
    o = [aesonQQ|{"b":[{"d":{"g":{"z":22},"b":34}}]}|]

subSelectSiblingNestedArrayField :: TestTree
subSelectSiblingNestedArrayField =
  mkTest "sibling and sub-select with nested field in array" $ TestCase e o "url,obj(url,a/url)"
  where
    e = [aesonQQ|{"url":1,"obj":{"url":"h","a":[{"url":1}]}}|]
    o = [aesonQQ|{"url":1,"id":"1","obj":{"url":"h","a":[{"url":1,"z":2}],"c":3}}|]

wildcardSubSelect :: TestTree
wildcardSubSelect = mkTest "wildcard sub-select" $ TestCase e o "*(a,b)"
  where
    e = [aesonQQ|{"p1":{"a":1,"b":1},"p2":{"a":2,"b":2}}|]
    o = [aesonQQ|{"p1":{"a":1,"b":1,"c":1},"p2":{"a":2,"b":2,"c":2}}|]

escapedField :: TestTree
escapedField = mkTest "escaped field" $ TestCase e o "a\\/b"
  where
    e = [aesonQQ|{"a/b":1}|]
    o = [aesonQQ|{"a/b":1,"c":2}|]

escapedWildcard :: TestTree
escapedWildcard = mkTest "escaped wildcard" $ TestCase e o "\\*"
  where
    e = [aesonQQ|{"*":101}|]
    o = [aesonQQ|{"*":101,"beta":"hidden"}|]
