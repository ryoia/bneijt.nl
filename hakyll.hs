{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Arrow ((>>>), (***), arr)
import Control.Category (id)
import Data.Monoid (mempty, mconcat, mappend)
import System.FilePath (splitPath, joinPath)

import Hakyll

main :: IO ()
main = hakyll $ do

    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Render posts
--    match "posts/*.markdown" $ do
--        route   $ setExtension ".html"
--        compile $ pandocCompiler >>= loadAndApplyTemplate "templates/post.html" postContext

    match "static/**" $ do
        route   $ dropPat "static/"
        compile $ copyFileCompiler

    match "templates/*" $ compile templateCompiler

dropPat :: String -> Routes
dropPat pat = gsubRoute pat (const "")

postContext :: Context String
postContext =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
