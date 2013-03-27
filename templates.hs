{-# LANGUAGE OverloadedStrings #-}

-- http://mschade.me/post/34014291111/html-conditional-comments-with-blaze-html
import           Text.Blaze.Internal
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T

import Data.Monoid (mempty)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

-- Overall template
basePage :: H.Html
basePage = H.docTypeHtml $ do
    H.head $ do
        --  Basic Page Needs
        --   ================================================== 
        H.meta ! charset "utf-8"
        H.title "Trailer Tracker"
        H.meta ! name "description" ! content "Informing and connecting people around the resale of over 150,000 potentially hazardous FEMA trailers"
        H.meta ! name "author" ! content "Nick Shapiro and Thomas Levine"
        --  Mobile Specific H.metas
        --   ================================================== 
        H.meta ! name "viewport" ! content "width=device-width, initial-scale=1, maximum-scale=1"
        --  CSS
        --   ================================================== 
        H.link ! rel "stylesheet" ! href "/stylesheets/base.css"
        H.link ! rel "stylesheet" ! href "/stylesheets/skeleton.css"
        H.link ! rel "stylesheet" ! href "/stylesheets/layout.css"
        --  Favicons
        -- 	================================================== 
        H.link ! rel "shortcut icon" ! href "/images/favicon.ico"
        H.link ! rel "apple-touch-icon" ! href "/images/apple-touch-icon.png"
        H.link ! rel "apple-touch-icon" ! sizes "72x72" ! href "/images/apple-touch-icon-72x72.png"
        H.link ! rel "apple-touch-icon" ! sizes "114x114" ! href "/images/apple-touch-icon-114x114.png"
    H.body $ do
        --  Primary Page Layout
        -- 	================================================== 
        --  Delete everything in this .container and get started on your own site! 
        H.div ! class_ "container" $ do
            H.header ! class_ "sixteen columns" $ do
                H.a ! href "/" $ H.h1 ! class_ "remove-bottom" $ "Trailer Tracker"
                H.form ! action "/logout" ! method "post" ! class_ "account" $ H.button ! type_ "submit" $ "Log out"
                H.a ! href "/login" ! class_ "account button" $ "Log in"
                H.a ! href "/me" ! class_ "account button" $ "Account"
                H.hr
            H.div ! A.id "body" $ mempty
            H.footer ! class_ "sixteen columns" $ do
                H.hr
                H.p "Read more."
        --  container 
        --  End Document
        -- ================================================== 


main = do
  print $ renderHtml basePage
