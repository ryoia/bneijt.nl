{-# LANGUAGE OverloadedStrings #-}
module Main where

import      Control.Applicative ((<$>))
import      Control.Arrow       (second)
import      Control.Monad       (forM_, forM)
import      Data.List           (isPrefixOf, isSuffixOf, sortBy)
import      Data.Map            (findWithDefault)
import      Data.Ord            (comparing)
import      Data.Monoid         (mappend)
import      Hakyll
import      Hakyll.Core.Configuration (defaultConfiguration)
import      System.FilePath     (dropTrailingPathSeparator, splitPath, takeBaseName, takeDirectory)
import      Text.Pandoc
import      System.Locale       (defaultTimeLocale)
-- import Debug.Trace (trace)

postsPattern = "blog/post/*/index.markdown"

main :: IO ()
main = hakyllWith hakyllConfig $ do

    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Render posts
    match postsPattern $ do
        route   $ setExtension ".html"
        compile $ pandocCompiler
            >>= saveSnapshot "body"
            >>= loadAndApplyTemplate "templates/post.html" postContext

    match "blog/post/**.jpg" $ do
        route   idRoute
        compile copyFileCompiler

    match "blog/post/**.py" $ do
        route   idRoute
        compile copyFileCompiler

    match "blog/post/**.xz" $ do
        route   idRoute
        compile copyFileCompiler

    match "blog/post/**.sh" $ do
        route   idRoute
        compile copyFileCompiler

    match "blog/post/**.png" $ do
        route   idRoute
        compile copyFileCompiler

    match "static/**" $ do
        route   $ dropPat "static/"
        compile $ copyFileCompiler

    match "templates/*" $ compile templateCompiler

    create ["blog/feed.atom"] $ do
        route idRoute
        compile $ do
            let feedCtx = postContext `mappend` bodyField "description"
            posts <- chronoFeed (loadAllSnapshots postsPattern "body")
            renderAtom myFeedConfiguration feedCtx posts

    -- Post list
    create ["blog/index.html"] $ do
        route idRoute
        compile $ do
            list <- postList postsPattern chronologicalItems
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"
                        (constField "title" "Posts" `mappend`
                            constField "posts" list `mappend`
                            defaultContext)
                >>= cleanIndexUrls


cleanIndexUrls :: Item String -> Compiler (Item String) 
cleanIndexUrls = return . fmap (withUrls clean) 
    where 
        idx = "index.html" 
        clean url 
            | idx `isSuffixOf` url = take (length url - length idx) url 
            | otherwise            = url


postList :: Pattern -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList pattern preprocess' = do
    postItemTpl <- loadBody "templates/postitem.html"
    posts       <- (loadAll pattern) >>= preprocess'
    applyTemplateList postItemTpl postContext posts

dropPat :: String -> Routes
dropPat pat = gsubRoute pat (const "")

chronoFeed :: Compiler [Item a] -> Compiler [Item a]
chronoFeed items = do
    ul <- items
    ol <- chronologicalItems ul
    return (take 10 ol)

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

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Bram Neijt, blog"
    , feedDescription = "Personal blog of Bram Neijt"
    , feedAuthorName  = "Bram Neijt"
    , feedAuthorEmail = "bneijt@gmail.com"
    , feedRoot        = "http://bneijt.nl"
    }

hakyllConfig :: Configuration
hakyllConfig = defaultConfiguration
  { deployCommand = "rsync --delete --recursive --progress _site/ bneijt.nl:/home/bram/vhost/bneijt.nl/_/"
  }
