{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as M

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

type UUID = String
type Datetime = Integer
type Snapthingy = ()

data TT
  = TT { _heist :: Snaplet (Heist TT)
       }
makeLenses ''TT

instance HasHeist TT where
  heistLens = subSnaplet heist


-- Question text, datetime, result
type QuestionAnswer = (String, Maybe Datetime, QuestionResponse)

-- These are the a types for the question/answer. They present the question too
data QuestionResponse
  = Radio ([String], Maybe String, [String])
  | Checkbox (M.Map String Bool)
  | Textbox String

-- Something handles the answer HTTP (string) response
type QuestionAsker    = QuestionAnswer -> Snapthingy
type QuestionListener = QuestionAnswer -> String -> QuestionAnswer

-- Saved
type Questionnaire      = M.Map String QuestionAnswer
type QuestionnaireTable = M.Map UUID Questionnaire

-- Make a question/answer
toQA :: String -> String -> QuestionResponse -> (String, QuestionAnswer)
toQA short_name question response = (short_name, (question, Nothing, response))

emptyTextbox :: QuestionResponse
emptyTextbox = Textbox ""

-- Make a check box thingy.
-- toCheckbox :: [String] -> Checkbox
toCheckbox choices = M.fromList $ map (\c -> (c, False)) choices

-- Make a radio button thingy.
toRadio :: [Text] -> QuestionResponse
toRadio choices = Radio (choices, Nothing, [])

activeObservationQuestions   = []
inactiveObservationQuestions = []

activeSymptomQuestions   = [ toQA "zipcode" "Zip Code" emptyTextbox
                           , toQA "email" "Email Address" emptyTextbox
                           , toQA "house.type" "Which of the following describes your FEMA housing unit?" $ toRadio ["Travel Trailer", "Park Model" "Mobile Home"]
                           , toQA "make" "What is the make of your housing unit?" emptyTextbox
                           , toQA "vin" "What is the VIN number or HUD number of your housing unit? (pop out on where they can find these numbers and what the difference between the two are)" emptyTextbox
                           , toQA "tested" "Have you tested your trailer for formaldehyde before?" emptyTextbox
                           , toQA "how.many.people" "How many people live in the FEMA trailer or mobile home with you?" emptyTextbox
                           , toQA "air.quality" "What concerns do you have about the quality of the air in your FEMA trailer?" emptyTextbox
                           , toQA "health.issues" "Have you developed any health issues since moving into the FEMA trailer?" emptyTextbox
                           , toQA "symptoms" "If so, what symptoms are your symptoms?" emptyTextbox
                           , toQA "symptom.causes" "Which of these symptoms do you believe are NOT caused by air quality issues? Please list the symptom and what you think it might be caused by." emptyTextbox
                           , toQA "respiratory" "Does anyone in your home have asthma or other respiratory health issues? If so, how severe were these issues before living in the trailer? and after moving into the trailer?" emptyTextbox
                           , toQA "other.effects" "Are there other effects of the air in your home on your health and well-being (social—embarrassment, not inviting people over or psychological—nonstop worrying, depression)?" emptyTextbox
                           , toQA "change.in.health" "Please compare your overall health before moving into the mobile home to your current health." emptyTextbox
                           , toQA "off.gassing" "Do you worry about off-gassing from specific components of your home? If so please describe." emptyTextbox
                           , toQA "change.in.air.quality" "Does the quality of your indoor air change from time to time? If so what leads to changes in indoor air-quality?" emptyTextbox
                           ]
inactiveSymptomQuestions = []

-- observationQuestionnaire = M.fromList $ inactiveObservationQuestions ++ activeObservationQuestions
-- symptomQuestionnaire     = M.fromList $ inactiveSymptomQuestions ++ activeSymptomQuestions

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
