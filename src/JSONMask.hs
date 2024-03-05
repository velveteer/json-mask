{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module JSONMask (
  Mask,
  Prop (..),
  Name,
  parseMask,
  renderMask,
  maskValue,
  masked,
) where

import Control.Applicative ((<|>))
import Control.Lens (Traversal', asIndex, at, non, (&), (.~), (?~), (^..), (^?))
import Control.Monad (when)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Lens (key, members, _Object)
import Data.Attoparsec.Text qualified as A
import Data.List (foldl', intersperse)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder qualified as TL

type Mask = Props
type Props = [Prop]
type PropTree = (Name, Props)
type Name = Text

data Prop
  = PropObject PropTree
  | PropArray PropTree
  deriving (Show)

getPropTree :: Prop -> PropTree
getPropTree = \case PropObject t -> t; PropArray t -> t;

pattern PropTree :: PropTree -> Prop
pattern PropTree tree <- (getPropTree -> tree)
{-# COMPLETE PropTree #-}

renderMask :: Mask -> Text
renderMask =
  toStrict
    . TL.toLazyText
    . mconcat
    . intersperse (TL.singleton ',')
    . foldMap go
 where
  go :: Prop -> [TL.Builder]
  go = \case
    PropObject (name, props) ->
      [ TL.fromText name
          <> mconcat (fmap (TL.singleton '/' <>) (foldMap go props))
      ]
    PropArray (name, props) ->
      [ TL.fromText name
          <> TL.singleton '('
          <> mconcat (intersperse (TL.singleton ',') (foldMap go props))
          <> TL.singleton ')'
      ]

parseMask :: Text -> Either String Mask
parseMask txt
  | T.null (T.strip txt) = Right []
  | otherwise = A.parseOnly parseProps txt

parseProps :: A.Parser Props
parseProps = parseProp `A.sepBy'` A.char ','

parseProp :: A.Parser Prop
parseProp =
  PropArray <$> parseArray
    <|> PropObject <$> parseObject

parseArray :: A.Parser PropTree
parseArray =
  (,)
    <$> parseName
    <*> (A.char '(' *> parseProps <* A.char ')')

parseObject :: A.Parser PropTree
parseObject =
  (,)
    <$> parseName
    <*> A.option [] (A.char '/' *> fmap pure parseProp)

parseName :: A.Parser Name
parseName = do
  txt <- A.scan False go
  when (T.null txt) $ fail "invalid field"
  pure txt
 where
  go _ c | c == '\\' = Just True
  go False c | c `elem` terminals = Nothing
  go True c | c `elem` terminals = Just False
  go s _ = Just s

terminals :: [Char]
terminals = ['(', ')', ',', '/']

unescape :: Text -> Text
unescape = T.replace "\\" ""

isWildcard :: Text -> Bool
isWildcard = (==) "*"

setPath :: KM.Key -> Traversal' Aeson.Value Aeson.Value
setPath path = _Object . at path . non (Aeson.Object mempty)

-- | Mask a `Value` and treat parse errors as a no-op.
masked :: Text -> Aeson.Value -> Aeson.Value
masked t v = either (const v) (`maskValue` v) (parseMask t)

maskValue :: Mask -> Aeson.Value -> Aeson.Value
maskValue [] v = v
maskValue m v = case v of
  Aeson.Array vec ->
    Aeson.Array $ maskValue m <$> vec
  (Aeson.Object _) ->
    foldl' go (Aeson.Object mempty) m
  val -> val
 where
  go !acc = \case
    PropTree (name, props) -> maskValue' name props acc v

maskValue' :: Name -> Props -> Aeson.Value -> Aeson.Value -> Aeson.Value
maskValue' name props acc current = do
  let path = K.fromText (unescape name)
      value = current ^? key path
  if isWildcard name
    then maskWildcard current props
    else case value of
      Just v -> acc & setPath path .~ maskValue props v
      _ -> acc

maskWildcard :: Aeson.Value -> Mask -> Aeson.Value
maskWildcard current props =
  case current of
    object@(Aeson.Object _) -> do
      let allKeys = object ^.. members . asIndex
      foldl'
        ( \o k -> case current ^? key k of
            Just v@(Aeson.Object _) -> o & _Object . at k ?~ maskValue props v
            Just v@(Aeson.Array _) -> o & setPath k .~ maskValue props v
            Just val | null props -> o & setPath k .~ val
            _ -> o
        )
        (Aeson.Object mempty)
        allKeys
    _ -> Aeson.Object mempty
