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


-- Question text, datetime, result
type QuestionAnswer a = (String, Maybe Datetime, a)

-- These are the a types for the question/answer. They present the question too
type Radio = ([String], Maybe String, [String])
type Checkbox = M.Map String Bool
type Textbox = String

-- Something handles the answer HTTP (string) response
type QuestionAsker a    = QuestionAnswer a -> Snapthingy
type QuestionListener a = QuestionAnswer a -> String -> QuestionAnswer a

-- Saved
type QuestionnaireTable = M.Map UUID Questionnaire
type Questionnaire      = M.Map String QuestionAnswer

-- Make a question/answer
toQA :: String -> String -> a -> QuestionAnswer a
toQA short_name question responses = (short_name, (question, Nothing, responses))

-- Make a check box thingy.
toCheckbox :: [String] -> Checkbox
toCheckbox choices = M.fromList $ map (\c -> (c, False)) choices

-- Make a radio button thingy.
toCheckbox :: [String] -> Radio
toCheckbox choices = (choices, Nothing, [])

inactiveObservationQuestions = []
activeObservationQuestions   = []

inactiveSymptomQuestions = []
activeSymptomQuestions   = [ toQA "zipcode" "Zip Code" ""
                           , toQA "email" "Email Address" ""
                           , toQA "house.type" "Which of the following describes your FEMA housing unit?" $ toCheckbox ["Travel Trailer", "Park Model" "Mobile Home"]
                           , toQA "make" "What is the make of your housing unit?" ""
                           , toQA "vin" "What is the VIN number or HUD number of your housing unit? (pop out on where they can find these numbers and what the difference between the two are)" ""
                           , toQA "tested" "Have you tested your trailer for formaldehyde before?" ""
                           , toQA "how.many.people" "How many people live in the FEMA trailer or mobile home with you?" ""
                           , toQA "air.quality" "What concerns do you have about the quality of the air in your FEMA trailer?" ""
                           , toQA "health.issues" "Have you developed any health issues since moving into the FEMA trailer?" ""
                           , toQA "symptoms" "If so, what symptoms are your symptoms?" ""
                           , toQA "symptom.causes" "Which of these symptoms do you believe are NOT caused by air quality issues? Please list the symptom and what you think it might be caused by." ""
                           , toQA "respiratory" "Does anyone in your home have asthma or other respiratory health issues? If so, how severe were these issues before living in the trailer? and after moving into the trailer?" ""
                           , toQA "other.effects" "Are there other effects of the air in your home on your health and well-being (social—embarrassment, not inviting people over or psychological—nonstop worrying, depression)?" ""
                           , toQA "change.in.health" "Please compare your overall health before moving into the mobile home to your current health." ""
                           , toQA "off.gassing" "Do you worry about off-gassing from specific components of your home? If so please describe." ""
                           , toQA "change.in.air.quality" "Does the quality of your indoor air change from time to time? If so what leads to changes in indoor air-quality?" ""
                           ]

observationQuestionnaire = M.fromList $ inactiveObservationQuestions ++ activeObservationQuestions
symptomQuestionnaire     = M.fromList $ inactiveSymptomQuestions ++ activeSymptomQuestions

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
