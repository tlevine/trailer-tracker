import Snap
import Snap.Snaplet.Heist
import Snap.Util.FileServe
-- import Snap.Extras.CoreUtils
-- import Snap.Extras.TextUtils
import Control.Lens
-- import Data.Text
-- import Data.Text.Encoding
import Data.Monoid
import Heist.Interpreted

data TT
  = TT { _heist :: Snaplet (Heist TT)
       }
makeLenses ''TT

instance HasHeist TT where
  heistLens = subSnaplet heist

indexHandler :: Handler TT TT ()
indexHandler = do
  render "index"

questionnaireHandler :: Handler TT TT ()
questionnaireHandler = do
  render "questionnaire"

observationHandler :: Handler TT TT ()
observationHandler = do
  render "observation"

ttInit :: SnapletInit TT TT
ttInit = makeSnaplet "Trailer Tracker" "Track inhabitable FEMA trailers" Nothing $ do
  h <- nestSnaplet "heist" heist $ heistInit "templates"
  -- modifyHeistState $ bindAttributeSplices [("main-textbox", mainTextboxAttributeSplice)]
  addRoutes [ ("images", serveDirectory "static/images")
            , ("stylesheets", serveDirectory "static/stylesheets")
            , ("questionnaires/a", questionnaireHandler)
            , ("observations/a", observationHandler)
            , ("", indexHandler)
            ]
  return $ TT { _heist = h
              }

main :: IO ()
main = do
  (_, site, _) <- runSnaplet Nothing ttInit
  quickHttpServe site
