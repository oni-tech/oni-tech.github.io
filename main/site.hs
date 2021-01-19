{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.List (intersperse, sortBy)
import Data.Text (pack)
import Hakyll
import Hakyll.Web.Feed (renderAtom, renderRss)
import Text.Blaze.Html (toHtml, toValue, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  -- Build up tags
  tags <- buildTags "posts/*" (fromCapture "tags/*.html")
  tagsRules tags $ \tag pattern -> do
    let title = "Posts tagged \"" ++ tag ++ "\""
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let ctx =
            constField "title" title
              <> listField "posts" (postCtxWithTags tags) (return posts)
              <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  -- Build individual posts pages
  match "posts/*" $ do
    route $ setExtension "html"
    compile $ do
      pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" (postCtxWithTags tags)
        >>= relativizeUrls

  -- Create HTML redirects for old url structure
  createRedirects
    [ ( "blog/2009/05/23/acer-aspire-one-with-netbsd-50/index.html",
        "/posts/2009-05-23-acer-aspire-one-with-netbsd-50.html"
      )
    ]

  -- Build main index.html page
  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (return posts)
              <> constField "title" "Home"
              <> defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/index.html" indexCtx
        >>= relativizeUrls

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
  dateField "date" "%B %e, %Y" <> defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags =
  tagsFieldWithClass "tags" tags <> postCtx

tagsFieldWithClass :: String -> Tags -> Context a
tagsFieldWithClass =
  tagsFieldWith getTags simpleRenderLink (mconcat . intersperse " ")
  where
    simpleRenderLink :: String -> (Maybe FilePath) -> Maybe H.Html
    simpleRenderLink tag = \case
      Nothing -> Nothing
      Just filePath ->
        Just $ H.a ! A.href (toValue $ toUrl filePath) ! A.class_ "tag" $ toHtml tag

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
