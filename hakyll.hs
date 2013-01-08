{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative ((<$>))
import           Control.Arrow       (second)
import           Control.Monad       (forM_)
import           Data.List           (isPrefixOf)
import           Data.Monoid         (mappend)
import           Hakyll
import           System.FilePath     (dropTrailingPathSeparator, splitPath)
import           Text.Pandoc

main :: IO ()
main = hakyll $ do

    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Render posts
    match "blog/post/*.markdown" $ do
        route   $ setExtension ".html"
        compile $ pandocCompiler >>= loadAndApplyTemplate "templates/post.html" postContext

    match "static/**" $ do
        route   $ dropPat "static/"
        compile $ copyFileCompiler

    match "templates/*" $ compile templateCompiler

    -- Post list
    create ["blog/index.html"] $ do
        route idRoute
        compile $ do
            list <- postList "blog/post/*" recentFirst
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

dropPat :: String -> Routes
dropPat pat = gsubRoute pat (const "")

postContext :: Context String
postContext =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
