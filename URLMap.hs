{-# LANGUAGE OverloadedStrings #-}

module URLMap (parseURLMap, URL, URLMap) where

import Control.Applicative ((<$>),(<$),(<*),(<*>),(*>),pure)
import qualified Data.ByteString.Char8 as S
import Network.Web.Server.Basic
import Network.Web.URI
import Text.Parsec
import Text.Parsec.String

type URL = String
type URLMap = [(URL,Path)]

parseURLMap :: String -> URLMap
parseURLMap cs = either (fail . show) (map fixCGI) (parse umap "umap" cs)
  where
    fixCGI (k, CGI prog param _) = (k, CGI prog param (scriptDir k))
    fixCGI kv                    = kv
    scriptDir x = maybe "" (S.unpack . uriPath) $ parseURI $ S.pack x

umap :: Parser [(URL,Path)]
umap =  comments *> many (line <* comments)

comments :: Parser ()
comments = () <$ many comment

comment :: Parser ()
comment = () <$ char '#' >> many (noneOf "\n") >> eol

line :: Parser (URL,Path)
line = (,) <$> uri <*> (fileOrCGI <* eol)

uri :: Parser URL
uri = spcs *> (str <* spcs)

fileOrCGI :: Parser Path
fileOrCGI = file <|> cgi

file :: Parser Path
file = File <$> (arrow *> dir)

cgi :: Parser Path
cgi = CGI <$> (darrow *> dir) <*> pure "" <*> pure ""

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
