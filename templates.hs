{-# LANGUAGE OverloadedStrings #-}

-- http://mschade.me/post/34014291111/html-conditional-comments-with-blaze-html
import           Text.Blaze.Internal
import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T

import Data.Monoid (mempty)
import Text.Blaze.Renderer.Utf8 (renderHtml)

-- Overall template
basePage :: Html
basePage = docTypeHtml $ do
    H.head $ do
        --  Basic Page Needs
        --   ================================================== 
        meta ! charset "utf-8"
        H.title "Trailer Tracker"
        meta ! name "description" ! content "Informing and connecting people around the resale of over 150,000 potentially hazardous FEMA trailers"
        meta ! name "author" ! content "Nick Shapiro and Thomas Levine"
        --  Mobile Specific Metas
        --   ================================================== 
        meta ! name "viewport" ! content "width=device-width, initial-scale=1, maximum-scale=1"
        --  CSS
        --   ================================================== 
        link ! rel "stylesheet" ! href "/stylesheets/base.css"
        link ! rel "stylesheet" ! href "/stylesheets/skeleton.css"
        link ! rel "stylesheet" ! href "/stylesheets/layout.css"
        --  Favicons
        -- 	================================================== 
        link ! rel "shortcut icon" ! href "/images/favicon.ico"
        link ! rel "apple-touch-icon" ! href "/images/apple-touch-icon.png"
        link ! rel "apple-touch-icon" ! sizes "72x72" ! href "/images/apple-touch-icon-72x72.png"
        link ! rel "apple-touch-icon" ! sizes "114x114" ! href "/images/apple-touch-icon-114x114.png"
    body $ do
        --  Primary Page Layout
        -- 	================================================== 
        --  Delete everything in this .container and get started on your own site! 
        H.div ! class_ "container" $ do
            header ! class_ "sixteen columns" $ do
                a ! href "/" $ h1 ! class_ "remove-bottom" $ "Trailer Tracker"
                H.form ! action "/logout" ! method "post" ! class_ "account" $ button ! type_ "submit" $ "Log out"
                a ! href "/login" ! class_ "account button" $ "Log in"
                a ! href "/me" ! class_ "account button" $ "Account"
                hr
            H.div ! A.id "body" $ mempty
            footer ! class_ "sixteen columns" $ do
                hr
                p "Read more."
        --  container 
        --  End Document
        -- ================================================== 


main = do
  putStrLn $ "h" -- renderHtml basePage
