{-# LANGUAGE OverloadedStrings #-}

import           Text.Blaze.Internal
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Monoid (mempty)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

-- Overall template
basePage :: H.Html
basePage = H.docTypeHtml $ do
    H.head $ do
        --  Basic Page Needs
        --   ================================================== 
        H.meta ! A.charset "utf-8"
        H.title "Trailer Tracker"
        H.meta ! A.name "description" ! A.content "Informing and connecting people around the resale of over 150,000 potentially hazardous FEMA trailers"
        H.meta ! A.name "author" ! A.content "Nick Shapiro and Thomas Levine"
        --  Mobile Specific H.metas
        --   ================================================== 
        H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1, maximum-scale=1"
        --  CSS
        --   ================================================== 
        H.link ! A.rel "stylesheet" ! A.href "/stylesheets/base.css"
        H.link ! A.rel "stylesheet" ! A.href "/stylesheets/skeleton.css"
        H.link ! A.rel "stylesheet" ! A.href "/stylesheets/layout.css"
        --  Favicons
        -- 	================================================== 
        H.link ! A.rel "shortcut icon" ! A.href "/images/favicon.ico"
        H.link ! A.rel "apple-touch-icon" ! A.href "/images/apple-touch-icon.png"
        H.link ! A.rel "apple-touch-icon" ! A.sizes "72x72" ! A.href "/images/apple-touch-icon-72x72.png"
        H.link ! A.rel "apple-touch-icon" ! A.sizes "114x114" ! A.href "/images/apple-touch-icon-114x114.png"
    H.body $ do
        --  Primary Page Layout
        -- 	================================================== 
        --  Delete everything in this .container and get started on your own site! A.
        H.div ! A.class_ "container" $ do
            H.header ! A.class_ "sixteen columns" $ do
                H.a ! A.href "/" $ H.h1 ! A.class_ "remove-bottom" $ "Trailer Tracker"
                H.form ! A.action "/logout" ! A.method "post" ! A.class_ "account" $ H.button ! A.type_ "submit" $ "Log out"
                H.a ! A.href "/login" ! A.class_ "account button" $ "Log in"
                H.a ! A.href "/me" ! A.class_ "account button" $ "Account"
                H.hr
            H.div ! A.id "body" $ mempty
            H.footer ! A.class_ "sixteen columns" $ do
                H.hr
                H.p "Read more."
        --  container 
        --  End Document
        -- ================================================== 

main = do
  print $ renderHtml basePage
