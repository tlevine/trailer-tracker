{-# LANGUAGE OverloadedStrings #-}
module Templates
( index
, placeholder
) where

-- import qualified Data.Set as S
import qualified Data.Map as M


-- Templates
import           Text.Blaze                             (toValue)
import           Text.Blaze.Html                        (toHtml)
import			 Text.Blaze.Internal
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Data.Monoid                            (mempty)
import           Control.Monad                          (forM_)

-- Other
import qualified Data.UUID as U
import           Schema

-- Not really needed
import           Text.Blaze.Html.Renderer.Utf8          (renderHtml)


-- Overall template
base :: H.Html -> H.Html
base body = H.docTypeHtml $ do
  H.head $ do
    --	Basic Page Needs
    --	 ================================================== 
    H.meta ! A.charset "utf-8"
    H.title "Trailer Tracker"
    H.meta ! A.name "description" ! A.content "Informing and connecting people around the resale of over 150,000 potentially hazardous FEMA trailers"
    H.meta ! A.name "author" ! A.content "Nick Shapiro and Thomas Levine"
    --	Mobile Specific H.metas
    --	 ================================================== 
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1, maximum-scale=1"
    --	CSS
    --	 ================================================== 
    H.link ! A.rel "stylesheet" ! A.href "/stylesheets/base.css"
    H.link ! A.rel "stylesheet" ! A.href "/stylesheets/skeleton.css"
    H.link ! A.rel "stylesheet" ! A.href "/stylesheets/layout.css"
    --	Favicons
    --	================================================== 
    H.link ! A.rel "shortcut icon" ! A.href "/images/favicon.ico"
    H.link ! A.rel "apple-touch-icon" ! A.href "/images/apple-touch-icon.png"
    H.link ! A.rel "apple-touch-icon" ! A.sizes "72x72" ! A.href "/images/apple-touch-icon-72x72.png"
    H.link ! A.rel "apple-touch-icon" ! A.sizes "114x114" ! A.href "/images/apple-touch-icon-114x114.png"
  H.body $ do
    --	Primary Page Layout
    --	================================================== 
    --	Delete everything in this .container and get started on your own site! A.
    H.div ! A.class_ "container" $ do
      H.header ! A.class_ "sixteen columns" $ do
        H.a ! A.href "/" $ H.h1 ! A.class_ "remove-bottom" $ "Trailer Tracker"
        H.form ! A.action "/logout" ! A.method "post" ! A.class_ "account" $ H.button ! A.type_ "submit" $ "Log out"
        H.a ! A.href "/login" ! A.class_ "account button" $ "Log in"
        H.a ! A.href "/me" ! A.class_ "account button" $ "Account"
        H.hr
      H.div ! A.id "body" $ body
      H.footer ! A.class_ "sixteen columns" $ do
        H.hr
        H.p $ do
          H.a ! A.href "#" $  "Read more"
          "."
    --	container 
    --	End Document
    -- ================================================== 

-- /
index :: U.UUID -> H.Html
index uuid = base $ do
  H.div ! A.class_ "actions" $ do
    H.div ! A.class_ "one-third column" $ do
      H.a ! A.href trailerLink ! A.class_ "button" $ "Add a trailer"
    H.div ! A.class_ "one-third column" $ do
      H.a ! A.href symptomLink ! A.class_ "button" $ "Report symptoms"
    H.div ! A.class_ "one-third column" $ do
      H.a ! A.href "#" ! A.class_ "button" $ "Discuss"
  H.div ! A.class_ "sixteen columns" ! A.id "map" ! A.style "height: 40em; background-color: green;" $ mempty
  where
    uuidString = U.toString uuid
    symptomLink = toValue $ "/symptoms/" ++ uuidString
    trailerLink = toValue $ "/trailers/" ++ uuidString


-- A question for /track
oneQuestion :: String -> String -> QuestionResponse -> H.Html
oneQuestion questionCode questionText (Radio (before, selected, after)) = do
  H.label ! A.for (toValue questionCode) $ toHtml questionText
  H.select $ do
  --forM_ (option False) before
    option True selected
  --forM_ (option False) after
  where
    optionBase True  = H.option ! A.selected ""
    optionBase False = H.option 
    option selected value = optionBase selected ! A.name (toValue questionCode) ! A.value (toValue value) $ toHtml value

{-
oneQuestion questionCode questionText (Checkbox checks) = do
  H.label ! A.for (toValue questionCode) $ toHtml questionText
  -- forM_ checkbox $ M.toAscList checks
  where
    checkboxBase True          = H.input ! A.checked ""
    checkboxBase False         = H.input 
    checkbox (value, selected) = (checkboxBase selected) ! A.type_ "checkbox" ! A.name (toValue questionCode ) ! A.value (toValue value) $ toHtml value
-}

oneQuestion questionCode questionText (Textarea answerText) = do
  H.label ! A.for (toValue questionCode ) $ toHtml questionText
  H.textarea ! A.name (toValue questionCode ) $ toHtml answerText

-- A placeholder for prototyping
placeholder :: String -> H.Html
placeholder text = base $ do H.p $ toHtml text

main = do
  print $ renderHtml $ base mempty
