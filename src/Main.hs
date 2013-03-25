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


type Answer k = (Datetime, k)

type Question = (String, String -> k) ???
How do people automatically generate forms in Snap???

-- Saved
type QuestionnaireTable = M.Map UUID Questionnaire
type Questionnaire = M.Map String Answer

inactiveObservationQuestions = [
                               ]
activeObservationQuestions   = [
                               ]

inactiveSymptomQuestions = [
                           ]
activeSymptomQuestions   = [ ("zipcode", "Zip Code")
                           , ("email", "Email Address")
                           , ("house.type", "Which of the following describes your FEMA housing unit?")
                              -- 1.Travel Trailer
                              -- 2. Park Model
                              -- 3. Mobile Home
                           , ("make", "What is the make of your housing unit?")
                           , ("vin", "What is the VIN number or HUD number of your housing unit? (pop out on where they can find these numbers and what the difference between the two are)")
                           , ("tested", "Have you tested your trailer for formaldehyde before?")
                           , ("how.many.people", "How many people live in the FEMA trailer or mobile home with you?")
                           , ("air.quality", "What concerns do you have about the quality of the air in your FEMA trailer?")
                           , ("health.issues", "Have you developed any health issues since moving into the FEMA trailer?")
                           , ("symptoms", "If so, what symptoms are your symptoms?")
                           , ("symptom.causes", "Which of these symptoms do you believe are NOT caused by air quality issues? Please list the symptom and what you think it might be caused by.")
                           , ("respiratory", "Does anyone in your home have asthma or other respiratory health issues? If so, how severe were these issues before living in the trailer? and after moving into the trailer?")
                           , ("other.effects", "Are there other effects of the air in your home on your health and well-being (social—embarrassment, not inviting people over or psychological—nonstop worrying, depression)?")
                           , ("change.in.health", "Please compare your overall health before moving into the mobile home to your current health.")
                           , ("off.gassing", "Do you worry about off-gassing from specific components of your home? If so please describe.")
                           , ("change.in.air.quality", "Does the quality of your indoor air change from time to time? If so what leads to changes in indoor air-quality?")
                           ]

observationQuestionnaire = M.fromList $ inactiveObservationQuestions ++ activeObservationQuestions
symptomQuestionnaire = M.fromList $ inactiveSymptomQuestions ++ activeSymptomQuestions

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
