{-# LANGUAGE QuasiQuotes #-}

module Container.RegistryApiSpec (spec) where

import Container.Docker.OciManifest (OciManifestConfig (configDigest), OciManifestV2 (ociConfig))
import Container.Docker.SourceParser (RegistryImageSource, RepoDigest (RepoDigest), parseImageUrl)
import Control.Carrier.ContainerRegistryApi.Authorization (RegistryAuthChallenge (..), RegistryBearerChallenge (..), parseAuthChallenge)
import Control.Effect.ContainerRegistryApi (ContainerRegistryApi, getImageManifest)
import Control.Effect.Diagnostics (Diagnostics, Has, fromEitherShow)
import Control.Effect.Lift (Lift)
import Data.Text (Text)
import Data.Void (Void)
import Test.Effect (it', shouldBe')
import Test.Hspec (Expectation, Spec, describe, it)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (Parsec, parse)
import Text.Megaparsec.Error (ParseErrorBundle)
import Text.RawString.QQ (r)

wwwAuthenticate :: Text
wwwAuthenticate = [r|Bearer realm="https://quay.io/v2/auth",service="quay.io",scope="repository:some-repo/some-img:pull"|]

wwwAuthenticateBasic :: Text
wwwAuthenticateBasic = [r|Basic realm="https://quay.io/v2/auth",service="quay.io"|]

parseMatch :: (Show a, Eq a) => Parsec Void Text a -> Text -> a -> Expectation
parseMatch parser input expected = parse parser mempty input `shouldParse` expected

decodeStrict :: Text -> Text -> Either (ParseErrorBundle Text Void) RegistryImageSource
decodeStrict arch = parse (parseImageUrl arch) mempty

getImageConfig ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has ContainerRegistryApi sig m
  ) =>
  Text ->
  Text ->
  m RepoDigest
getImageConfig arch img =
  (configDigest . ociConfig)
    <$> (getImageManifest =<< fromEitherShow (decodeStrict arch img))

spec :: Spec
spec = do
  registryApiSpec
  parseAuthChallengeSpec

parseAuthChallengeSpec :: Spec
parseAuthChallengeSpec =
  describe "parseAuthChallenge" $ do
    let shouldParseInto = parseMatch parseAuthChallenge
    it "should parse bearer auth challenge" $
      wwwAuthenticate
        `shouldParseInto` BearerAuthChallenge
          ( RegistryBearerChallenge
              "https://quay.io/v2/auth"
              "quay.io"
              "repository:some-repo/some-img:pull"
          )

    it "should parse basic auth challenge" $
      wwwAuthenticateBasic
        `shouldParseInto` ( BasicAuthChallenge "https://quay.io/v2/auth"
                          )

registryApiSpec :: Spec
registryApiSpec =
  describe "Container Registry API" $
    describe "Public Registry APIs" $ do
      describe "Custom Registry" $ do
        it' "should get manifest with tag" $ do
          confDigest <- getImageConfig amd64 githubImage
          confDigest `shouldBe'` githubImageConfigDigest

        it' "should get manifest with digest" $ do
          confDigest <- getImageConfig amd64 githubImageWithDigest
          confDigest `shouldBe'` githubImageConfigDigest

        it' "should get manifest for multi-platform image (chooses target platform)" $ do
          confDigest <- getImageConfig amd64 githubMultiArchImage
          confDigest `shouldBe'` githubMultiArchImageConfigDigest

      describe "Default Index (docker)" $ do
        it' "should get manifest with tag" $ do
          confDigest <- getImageConfig amd64 dhImage
          confDigest `shouldBe'` dhImageDigest

        it' "should get manifest with digest" $ do
          confDigest <- getImageConfig amd64 dhImageWithDigest
          confDigest `shouldBe'` dhImageDigest

        it' "should get manifest for multi-platform image (chooses target platform)" $ do
          confDigest <- getImageConfig arm dhMultiArchImage
          confDigest `shouldBe'` dhMultiArchImageDigest

amd64 :: Text
amd64 = "amd64"

arm :: Text
arm = "arm"

githubImage :: Text
githubImage = "ghcr.io/fossas/haskell-dev-tools:8.10.4"

githubImageConfigDigest :: RepoDigest
githubImageConfigDigest =
  RepoDigest "sha256:e83e5c9e32de2454ef584b4582f7ea171c9c71b5f23c8f13e297a9153e02f4af"

githubImageWithDigest :: Text
githubImageWithDigest =
  "ghcr.io/fossas/haskell-dev-tools@sha256:35643079905dfdd597fa2cff3b46aadf5378d325a68f00bdb2665c49fa6ca6a2"

githubMultiArchImage :: Text
githubMultiArchImage =
  "ghcr.io/graalvm/graalvm-ce:ol7-java11-21.3.3"

githubMultiArchImageConfigDigest :: RepoDigest
githubMultiArchImageConfigDigest =
  RepoDigest "sha256:bdcba07acb11053fea0026b807ecf94550ace7df27b10596ca4c765165243cef"

dhImage :: Text
dhImage = "amazon/aws-cli:2.0.6"

dhImageDigest :: RepoDigest
dhImageDigest =
  RepoDigest "sha256:af825d93886a9ce8af4b7b9d36f3becbadef7420f79d2b05a3178ec0ad9e1a21"

dhImageWithDigest :: Text
dhImageWithDigest =
  "amazon/aws-cli@sha256:7a27c26c2937a3d0b84171675709df1dc09aa331e86cad90f74ada6df7b59c89"

dhMultiArchImage :: Text
dhMultiArchImage = "grafana/grafana:8.1.7-ubuntu"

dhMultiArchImageDigest :: RepoDigest
dhMultiArchImageDigest =
  RepoDigest "sha256:86618e1e78e4962b5abec6cc7fabe89010ebfbbf0885cbba1aada7287457c263"
