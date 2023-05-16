{-# LANGUAGE QuasiQuotes #-}

module Python.SetupPySpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Text.URI.QQ (uri)

import DepTypes (DepType (PipType), Dependency (..), VerConstraint (CAnd, CGreaterOrEq, CLess, CURI))
import Effect.Grapher (direct, evalGrapher, run)
import Graphing (Graphing)
import Strategy.Python.Util (Operator (OpGtEq, OpLt), Req (..), Version (Version), buildGraph)

import Data.Text (Text)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Text.RawString.QQ (r)

import Data.Void (Void)
import Strategy.Python.SetupPy (installRequiresParser)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (Parsec, parse)

parseMatch :: (Show a, Eq a) => Parsec Void Text a -> Text -> a -> Expectation
parseMatch parser input parseExpected = parse parser "" input `shouldParse` parseExpected

setupPyInput :: [Req]
setupPyInput =
  [ NameReq
      "pkgOne"
      Nothing
      ( Just
          [ Version OpGtEq "1.0.0"
          , Version OpLt "2.0.0"
          ]
      )
      Nothing
  , NameReq "pkgTwo" Nothing Nothing Nothing
  , UrlReq "pkgThree" Nothing [uri|https://example.com|] Nothing
  ]

expected :: Graphing Dependency
expected = run . evalGrapher $ do
  direct $
    Dependency
      { dependencyType = PipType
      , dependencyName = "pkgOne"
      , dependencyVersion =
          Just
            ( CAnd
                (CGreaterOrEq "1.0.0")
                (CLess "2.0.0")
            )
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = Map.empty
      }
  direct $
    Dependency
      { dependencyType = PipType
      , dependencyName = "pkgTwo"
      , dependencyVersion = Nothing
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = Map.empty
      }
  direct $
    Dependency
      { dependencyType = PipType
      , dependencyName = "pkgThree"
      , dependencyVersion = Just (CURI "https://example.com")
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = Map.empty
      }

spec :: Spec
spec = do
  describe "parse" $ do
    it "should parse setup.py without comments" $ do
      let shouldParseInto = parseMatch installRequiresParser
      setupPyWithoutComment `shouldParseInto` [mkReq "PyYAML", mkReq "pandas", mkReq "numpy"]
      setupPyWithoutComment2 `shouldParseInto` [mkReq "PyYAML", mkReq "pandas", mkReq "numpy"]

    it "should parse setup.py with backslash" $ do
      let shouldParseInto = parseMatch installRequiresParser
      setupPyWithBackslash `shouldParseInto` [mkReq "PyYAML", mkReq "pandas", mkReq "numpy"]

    it "should parse setup.py with comments" $ do
      let shouldParseInto = parseMatch installRequiresParser

      setupPyWithCommentAfterComma `shouldParseInto` [mkReq "PyYAML", mkReq "pandas", mkReq "numpy"]
      setupPyWithCommentBeforeReq `shouldParseInto` [mkReq "PyYAML", mkReq "numpy"]
      setupPyWithAllComments `shouldParseInto` []

  describe "analyze" $
    it "should produce expected output" $ do
      let result = buildGraph setupPyInput

      result `shouldBe` expected

mkReq :: Text -> Req
mkReq name = NameReq name Nothing Nothing Nothing

setupPyWithoutComment :: Text
setupPyWithoutComment =
  [r|from setuptools import setup, find_packages
setup(
    install_requires=[
        'PyYAML',
        'pandas',
        'numpy'
    ],
)
|]

setupPyWithoutComment2 :: Text
setupPyWithoutComment2 =
  [r|from setuptools import setup, find_packages
setup(
    install_requires=[
        'PyYAML',
        'pandas',
        'numpy',
    ],
)
|]

setupPyWithCommentAfterComma :: Text
setupPyWithCommentAfterComma =
  [r|from setuptools import setup, find_packages
setup(
    install_requires=[
        'PyYAML', # should not fail
        'pandas',
        'numpy'
    ],
)
|]

setupPyWithCommentBeforeReq :: Text
setupPyWithCommentBeforeReq =
  [r|from setuptools import setup, find_packages
setup(
    install_requires=[
        'PyYAML', # should not fail
        # 'pandas==0.23.3',
        'numpy'
    ],
)
|]

setupPyWithAllComments :: Text
setupPyWithAllComments =
  [r|from setuptools import setup, find_packages
setup(
    install_requires=[
        # 'PyYAML',
        # 'pandas==0.23.3',
        # 'numpy>=1.14.5'
    ],
)
|]

setupPyWithBackslash :: Text
setupPyWithBackslash =
  [r|from setuptools import setup, find_packages
setup(
    install_requires=[ 'PyYAML', \
      'pandas', \
      'numpy'
    ],
)
|]
