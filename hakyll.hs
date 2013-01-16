{-# LANGUAGE OverloadedStrings #-}
module Main where

import      Control.Applicative ((<$>))
import      Control.Arrow       (second)
import      Control.Monad       (forM_, forM)
import      Data.List           (isPrefixOf, sortBy, concat)
import      Data.Map            (findWithDefault)
import      Data.Ord            (comparing)
import      Data.Monoid         (mappend)
import      Hakyll
import      System.FilePath     (dropTrailingPathSeparator, splitPath, takeBaseName, takeDirectory)
import      Text.Pandoc
import      System.Locale       (defaultTimeLocale)
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
            list <- postList "blog/post/*/index.markdown" chronologicalItems
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"
                        (constField "title" "Posts" `mappend`
                            constField "posts" list `mappend`
                            defaultContext)


postList :: Pattern -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList pattern preprocess' = do
    postItemTpl <- loadBody "templates/postitem.html"
    posts       <- (loadAll pattern) >>= preprocess'
    applyTemplateList postItemTpl postContext posts

dropPat :: String -> Routes
dropPat pat = gsubRoute pat (const "")

chronologicalItems :: [Item a] -> Compiler [Item a] 
chronologicalItems items = do 
    withTime <- forM items $ \item -> do 
        utc <- getItemUTC defaultTimeLocale $ itemIdentifier item 
        return (utc, item)
    return $ map snd $ reverse $ sortBy (comparing fst) withTime 

postContext :: Context String
postContext =
    dateField "humanizedPublished" "%B %e, %Y" `mappend`
    defaultContext



