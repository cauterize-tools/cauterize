{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Meta.Pretty where

import Cauterize.Meta.Types
import Text.PrettyPrint.Leijen.Text
import qualified Data.Text.Lazy as T
import Data.Word
import Numeric

prettyMeta :: Meta -> T.Text
prettyMeta (Meta n v tl dl sh  sv ts) =
  let banner = "meta-interface"
      h'  = parens $ "sha1" <+> text (T.pack $ show sh)
      ts' = parens $ nest 1 ("types" <$> vcat (map prettyType ts))
      rest = vcat [ parens $ "meta-variant" <+> integer v
                  , parens $ "type-length" <+> integer tl
                  , parens $ "data-length" <+> integer dl
                  , ts'
                  ]
      doc = parens $ nest 1 (banner <+> ((text n <+> text sv <+> h') <$> rest))
  in displayT . renderPretty 1 120 . pretty $ doc
  where

prettyType :: MetaType -> Doc
prettyType (MetaType n p) = parens $ "type" <+> text n
                                            <+> prettyPrefix p

prettyPrefix :: [Word8] -> Doc
prettyPrefix p = hcat $ map prettyWord8 p

prettyWord8 :: Word8 -> Doc
prettyWord8 w = let s = showHex w ""
                in case s of
                    [v] -> text $ T.pack ['0',v]
                    [_,_] -> text $ T.pack s
                    _ -> error "This should never happen for a Word8."

