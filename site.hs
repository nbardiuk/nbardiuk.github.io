{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad                  ( forM_ )
import           Data.Monoid                    ( mappend )
import           Hakyll

main :: IO ()
main = hakyll $ do

  match "CNAME" $ do
    route idRoute
    compile copyFileCompiler

  forM_ ["favicon.ico", "images/*"] $ \p -> match p $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "posts/*" $ do
    route $ setExtension "html"
    compile
      $   pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html" postCtx
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx = mconcat
            [ listField "posts" postCtx (return posts)
            , constField "title" ""
            , defaultContext]

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls


  match "templates/*" $ compile templateBodyCompiler

  version "redirects" $ createRedirects brokenLinks

  create ["feed.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = bodyField "description" `mappend` postCtx
      posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
      renderRss feedConfig feedCtx posts

  match "404.markdown" $ do
    route $ setExtension "html"
    compile
      $   pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

postCtx :: Context String
postCtx = mconcat
          [ dateField "humandate" "%B %e, %Y" 
          , dateField "machinedate" "%Y-%m-%dT%H:%M:%SZ" 
          , defaultContext]

brokenLinks :: [(Identifier, String)]
brokenLinks =
  [ ("java-monad/index.html"          , "/posts/java-monad.html")
  , ("game-of-live-clojure/index.html", "/posts/game-of-live-clojure.html")
  , ("java-sequence/index.html"       , "/posts/java-sequence.html")
  ]

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
  { feedTitle       = "Nazarii Bardiuk's blog"
  , feedDescription = "A personal blog"
  , feedAuthorName  = "Nazarii Bardiuk"
  , feedAuthorEmail = "nazarii@bardiuk.com"
  , feedRoot        = "https://nazarii.bardiuk.com"
  }
