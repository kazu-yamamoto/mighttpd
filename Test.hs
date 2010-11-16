{-# LANGUAGE OverloadedStrings #-}

module Test where

import Control.Applicative
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import URLMap
import File
import Network.Web.URI
import Network.Web.Server.Basic

tests :: [Test]
tests = [
    testGroup "mapper" [
         testCase "parse" test_parse
       , testCase "mapping" test_mapping
       ]
  ]

test_parse :: Assertion
test_parse = do
    m <- parseURLMap <$> readFile sampleFile
    m @?= sampleData
 where
   sampleFile = "sample.map"
   sampleData = [("http://example.org/ja/cgi-bin/",CICgi {progDir = "/export/www/cgi-bin/", pathInURL = "/ja/cgi-bin/"}),("http://example.org/mailman/",CICgi {progDir = "/usr/local/mailman/cgi-bin/", pathInURL = "/mailman/"}),("http://example.org/pipermail/",CIFile "/usr/local/mailman/archives/public/"),("http://example.org/icons/",CIFile "/usr/local/mailman/icons/"),("http://example.org/~user/",CIFile "/home/user/public_html/"),("http://example.org/",CIFile "/export/www/")]

test_mapping :: Assertion
test_mapping = do
    m <- parseURLMap <$> readFile sampleFile
    fileMapper m uri1 @?= path1
    fileMapper m uri2 @?= path2
    fileMapper m uri3 @?= path3
 where
   sampleFile = "sample.map"
   Just uri1 = parseURI "http://example.org/ja/cgi-bin/foo/bar/baz?param=x"
   Just uri2 = parseURI "http://example.org/ja/"
   Just uri3 = parseURI "http://example.org/ja/?param"
   path1 = PathCGI (CGI { progPath = "/export/www/cgi-bin/foo"
                        , scriptName = "/ja/cgi-bin/foo"
                        , pathInfo = "/bar/baz"
                        , queryString = "?param=x"})
   path2 = File "/export/www/ja/index.html"
   path3 = File "/export/www/ja/index.html"


----------------------------------------------------------------

main :: Assertion
main = defaultMain tests
