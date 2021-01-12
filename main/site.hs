{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll
import Hakyll.Web.Feed (renderAtom, renderRss)

main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "posts/*" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Home"
              `mappend` defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/index.html" indexCtx
        >>= relativizeUrls

  -- Build up tags
  tags <- buildTags "posts/*" (fromCapture "tags/*.html")

  -- Render atom / rss feeds
  create ["atom.xml"] $ do
    route idRoute
    compile $ do
      posts <- fmap (take 10) . recentFirst =<< loadAll "posts/*"
      renderAtom feedConfiguration postCtx posts

  create ["rss.xml"] $ do
    route idRoute
    compile $ do
      posts <- fmap (take 10) . recentFirst =<< loadAll "posts/*"
      renderRss feedConfiguration postCtx posts

  match "templates/*" $ compile templateBodyCompiler

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` defaultContext

feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
    { feedTitle = "OniTech Blog",
      feedDescription =
        "OniTech aims to share knowledge about functional\
        \programming, distribute best-practices based on our extensive experience and \
        \tell about various topics from beginner-friendly introductions to advanced \
        \type-level tricks.",
      feedAuthorName = "OniTech",
      feedAuthorEmail = "oni-tech@gmail.com",
      feedRoot = "https://oni-tech.com"
    }
