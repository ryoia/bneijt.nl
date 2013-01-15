{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative ((<$>))
import           Control.Arrow       (second)
import           Control.Monad       (forM_)
import           Data.List           (isPrefixOf, sortBy, concat)
import           Data.Map            (findWithDefault)
import           Data.Ord            (comparing)
import           Data.Monoid         (mappend)
import           Hakyll
import           System.FilePath     (dropTrailingPathSeparator, splitPath, takeBaseName, takeDirectory)
import           Text.Pandoc
import Data.Maybe (fromMaybe)
import Text.Regex (matchRegex, mkRegex)
-- import Debug.Trace (trace)


main :: IO ()
main = hakyll $ do

    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Render posts
    match "blog/post/*/index.markdown" $ do
        route   $ setExtension ".html"
        compile $ pandocCompiler >>= loadAndApplyTemplate "templates/post.html" postContext

    match "blog/post/**.jpg" $ do
        route   idRoute
        compile copyFileCompiler

    match "blog/post/**.png" $ do
        route   idRoute
        compile copyFileCompiler

    match "static/**" $ do
        route   $ dropPat "static/"
        compile $ copyFileCompiler

    match "templates/*" $ compile templateCompiler

    -- Post list
    create ["blog/index.html"] $ do
        route idRoute
        compile $ do
            list <- postList "blog/post/*/index.markdown" chronologicalItem
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"
                        (constField "title" "Posts" `mappend`
                            constField "posts" list `mappend`
                            defaultContext)

postList :: Pattern -> ([Item String] -> [Item String])
         -> Compiler String
postList pattern preprocess' = do
    postItemTpl <- loadBody "templates/postitem.html"
    posts       <- preprocess' <$> loadAll pattern
    applyTemplateList postItemTpl postContext posts

selectDateLine :: String -> String
selectDateLine body = concat (fromMaybe [""] (matchRegex (mkRegex "(<time[^>]+)") body))

dropPat :: String -> Routes
dropPat pat = gsubRoute pat (const "")

chronologicalDirectory :: [Item String] -> [Item String]
chronologicalDirectory = sortBy $ comparing $ takeBaseName . takeDirectory . toFilePath . itemIdentifier

dateFieldOfItem :: Item String -> String
dateFieldOfItem a = selectDateLine (itemBody a)

chronologicalItem :: [Item String] -> [Item String]
chronologicalItem = reverse . (sortBy $ comparing $ dateFieldOfItem)

postContext :: Context String
postContext =
    dateField "humanizedDate" "%B %e, %Y" `mappend`
    defaultContext
