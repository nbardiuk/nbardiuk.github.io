--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    tags <- buildTags postsPattern (fromCapture "tags/*.html")

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
            >>= relativizeUrls

    tagsRules tags $ \tag pattern -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let tagCtx =
                  listField "posts" (postCtx tags) (return posts)
                  `mappend` constField "title" ("Posts tagged \"" ++ tag ++ "\"")
                  `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" tagCtx
                >>= loadAndApplyTemplate "templates/default.html" tagCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" (postCtx tags) (return posts)
                    `mappend` constField "title" ""
                    `mappend` defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls


    match "templates/*" $ compile templateBodyCompiler

    version "redirects" $ createRedirects brokenLinks

    create ["feed.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx =
                   bodyField "description" `mappend`
                   postCtx tags
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
            renderRss feedConfig feedCtx posts


--------------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx tags = tagsField "tags" tags
      `mappend` dateField "date" "%B %e, %Y"
      `mappend` defaultContext

brokenLinks =
    [ ("java-monad/index.html", "/posts/2015-11-08-java-monad.html")
    , ("game-of-live-clojure/index.html", "/posts/2014-11-29-quil.html")
    , ("java-sequence/index.html", "/posts/2016-10-16-java-sequence.html")
    ]

feedConfig = FeedConfiguration {
     feedTitle       = "Nazarii Bardiuk's blog"
    ,feedDescription = "A personal blog"
    ,feedAuthorName  = "Nazarii Bardiuk"
    ,feedAuthorEmail = "nazarii@bardiuk.com"
    ,feedRoot        = "https://nazarii.bardiuk.com"
}

postsPattern = "posts/*" .&&. complement "posts/index.html"
