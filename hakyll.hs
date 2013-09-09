module Main where

import      Control.Applicative ((<$>))
import      Control.Arrow       (second)
import      Control.Monad       (forM_, forM)
import      Data.List           (isPrefixOf, isSuffixOf, sortBy)
import      Data.Map            (findWithDefault)
import      Data.Ord            (comparing)
import      Data.Monoid         (mappend)
import      Hakyll
import      Hakyll.Core.Identifier.Pattern (fromGlob)
import      Hakyll.Core.Identifier (fromFilePath)
import      Hakyll.Core.Configuration (defaultConfiguration)
import      System.FilePath     (dropTrailingPathSeparator, splitPath, takeBaseName, takeDirectory)
import      Text.Pandoc
import      System.Locale       (defaultTimeLocale)
-- import Debug.Trace (trace)

postsPattern :: String
postsPattern = "blog/post/*/index.markdown"

main :: IO ()
main = hakyllWith hakyllConfig $ do

    -- Compress CSS
    match (fromGlob "src/main/css/*.css") $ do
        route   $ dropPat "src/main/"
        compile compressCssCompiler

    match (fromGlob "src/main/css/*.scss") $ do
        route   $ composeRoutes (dropPat "src/main/") (setExtension "css")
        compile $ getResourceString >>=
            withItemBody (unixFilter "sass" ["-s", "--scss"]) >>=
            return . fmap compressCss

    -- Compress JS
    match (fromGlob "src/main/libs/*/*.js") $ do
        route   $ composeRoutes (dropPat "src/main/") (setExtension ".min.js")
        compile $ getResourceString >>=
            withItemBody (unixFilter "cat" []) >>=
            return . fmap compressCss

    -- Render posts
    match (fromGlob postsPattern) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "body"
            >>= loadAndApplyTemplate (fromFilePath "templates/post.html") postContext

    match (fromGlob (postsPattern ++ ".runghc")) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "body"
            >>= loadAndApplyTemplate (fromFilePath "templates/post.html") postContext

    match (fromGlob "static/**") $ do
        route   $ dropPat "static/"
        compile $ copyFileCompiler

    match (fromGlob "templates/*") $ compile templateCompiler

    create [fromFilePath "blog/feed.atom"] $ do
        route idRoute
        compile $ do
            let feedCtx = postContext `mappend` bodyField "description"
            selectedPosts <- chronoFeed (loadAllSnapshots (fromGlob postsPattern) "body")
            filteredPosts <- mapM replaceAllLinks selectedPosts
            renderAtom myFeedConfiguration feedCtx filteredPosts

    -- Post list
    create [fromFilePath "blog/index.html"] $ do
        route idRoute
        compile $ do
            list <- postList (fromGlob postsPattern) chronologicalItems
            makeItem ""
                >>= loadAndApplyTemplate (fromFilePath "templates/posts.html")
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

-- TODO: fmap (maybe empty toUrl) . getRoute . itemIdentifier
replaceAllLinks :: Item String -> Compiler (Item String)
replaceAllLinks item = do
    --let route = toUrl (getRoute (itemIdentifier item))
    (return . fmap (withUrls clean)) item
    where
        clean url 
            | not (isExternal url)  = ""
            | otherwise             = url



postList :: Pattern -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList pattern preprocess' = do
    postItemTpl <- loadBody (fromFilePath "templates/postitem.html")
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
