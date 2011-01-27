{-# LANGUAGE OverloadedStrings #-}

module URLMap (parseURLMap, URL, URLMap, ConvInfo(..)) where

import Control.Applicative hiding (many,optional,(<|>))
import qualified Data.ByteString.Char8 as S
import Network.Web.URI
import Text.Parsec
import Text.Parsec.String

type URL = String
data ConvInfo = CIFile !String | CICgi { progDir :: !String
                                       , pathInURL :: !String
                                       }
                deriving (Eq,Show)
type URLMap = [(URL,ConvInfo)]

parseURLMap :: String -> URLMap
parseURLMap cs = either (fail . show) (map fixCGI) (parse umap "umap" cs)
  where
    fixCGI (k, ci@(CICgi _ _)) = (k, ci { pathInURL = scriptPath k})
    fixCGI kv               = kv
    scriptPath x = maybe "" (S.unpack . uriPath) $ parseURI $ S.pack x

umap :: Parser URLMap
umap =  comments *> many (line <* comments)

comments :: Parser ()
comments = () <$ many comment

comment :: Parser ()
comment = () <$ char '#' >> many (noneOf "\n") >> eol

line :: Parser (URL,ConvInfo)
line = (,) <$> uri <*> (fileOrCGI <* eol)

uri :: Parser URL
uri = spcs *> (str <* spcs)

fileOrCGI :: Parser ConvInfo
fileOrCGI = file <|> cgi

file :: Parser ConvInfo
file = CIFile <$> (arrow *> dir)

cgi :: Parser ConvInfo
cgi = CICgi <$> (darrow *> dir) <*> pure ""

arrow :: Parser ()
arrow = () <$ string "->" >> spcs

darrow :: Parser ()
darrow = () <$ string "=>" >> spcs

dir :: Parser FilePath
dir =  str <* spcs

str :: Parser String
str = many1 (noneOf " \t\n")

eol :: Parser ()
eol = () <$ char '\n'

spcs :: Parser ()
spcs = () <$ many (oneOf " \t")
