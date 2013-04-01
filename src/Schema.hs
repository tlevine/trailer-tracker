{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Schema where

import qualified Data.Set as S
import qualified Data.Map as M

-- import Control.Lens
-- import Data.Text
-- import Data.Text.Encoding
import Data.Monoid

import Data.UUID.V4 (nextRandom)
import qualified Data.UUID as U

type Datetime = Integer

-- Question text, datetime, result
type QuestionAnswer = (String, Maybe Datetime, QuestionResponse)

-- These are the a types for the question/answer. They present the question too
data QuestionResponse
  = Radio ([String], String, [String])
  | Checkbox (M.Map String Bool)
  | Textarea String
  deriving (Show)

-- Something handles the answer HTTP (string) response
type QuestionAsker    = QuestionAnswer -> Snapthingy
type QuestionListener = QuestionAnswer -> String -> QuestionAnswer

-- A user links to its questionnaires.
data QuestionnaireId
  = TrailerId U.UUID
  | SymptomId U.UUID
newtype GuestKey = U.UUID

data User = User { email :: String
                 , phone :: String
                 , trailers :: S.Set TrailerId
                 , symptoms :: S.Set SymptomId
                 } deriving (Eq, Ord, Read, Show, Data, Typeable)

-- A questionnaire is a map of question codes to question answers.
type Questionnaire = M.Map String QuestionAnswer

-- Acidic types
type Users = M.Map Integer User
type Questionnaires = M.Map QuestionnaireId Questionnaire
type GuestKeys = M.Map GuestKey User

$(deriveSafeCopy 0 'base ''Users)
$(deriveSafeCopy 0 'base ''Questionnaires)
$(deriveSafeCopy 0 'base ''GuestKeys)


-----------------------------------------------------------
-- Questionnaire
-----------------------------------------------------------

-- Make an unanswered question/answer
toQA :: String -> String -> QuestionResponse -> (String, QuestionAnswer)
toQA short_name question response = (short_name, (question, Nothing, response))

-- Make an empty text area.
emptyTextarea :: QuestionResponse
emptyTextarea = Textarea ""

-- Make a check box thingy.
emptyCheckbox :: [String] -> QuestionResponse
emptyCheckbox choices = Checkbox $ M.fromList $ map (\c -> (c, False)) choices

-- Make a radio button thingy.
emptyRadio :: [String] -> QuestionResponse
emptyRadio (x:xs) = Radio ([], x, xs)

activeObservationQuestions   = []
inactiveObservationQuestions = []

activeSymptomQuestions   = [ toQA "zipcode" "Zip Code" emptyTextarea
                           , toQA "email" "Email Address" emptyTextarea
                           , toQA "house.type" "Which of the following describes your FEMA housing unit?" $ emptyRadio ["Travel Trailer", "Park Model", "Mobile Home"]
                           , toQA "make" "What is the make of your housing unit?" emptyTextarea
                           , toQA "vin" "What is the VIN number or HUD number of your housing unit? (pop out on where they can find these numbers and what the difference between the two are)" emptyTextarea
                           , toQA "tested" "Have you tested your trailer for formaldehyde before?" emptyTextarea
                           , toQA "how.many.people" "How many people live in the FEMA trailer or mobile home with you?" emptyTextarea
                           , toQA "air.quality" "What concerns do you have about the quality of the air in your FEMA trailer?" emptyTextarea
                           , toQA "health.issues" "Have you developed any health issues since moving into the FEMA trailer?" emptyTextarea
                           , toQA "symptoms" "If so, what symptoms are your symptoms?" emptyTextarea
                           , toQA "symptom.causes" "Which of these symptoms do you believe are NOT caused by air quality issues? Please list the symptom and what you think it might be caused by." emptyTextarea
                           , toQA "respiratory" "Does anyone in your home have asthma or other respiratory health issues? If so, how severe were these issues before living in the trailer? and after moving into the trailer?" emptyTextarea
                           , toQA "other.effects" "Are there other effects of the air in your home on your health and well-being (social—embarrassment, not inviting people over or psychological—nonstop worrying, depression)?" emptyTextarea
                           , toQA "change.in.health" "Please compare your overall health before moving into the mobile home to your current health." emptyTextarea
                           , toQA "off.gassing" "Do you worry about off-gassing from specific components of your home? If so please describe." emptyTextarea
                           , toQA "change.in.air.quality" "Does the quality of your indoor air change from time to time? If so what leads to changes in indoor air-quality?" emptyTextarea
                           ]
inactiveSymptomQuestions = []

observationQuestionnaire :: Questionnaire
observationQuestionnaire = M.fromList $ inactiveObservationQuestions ++ activeObservationQuestions

symptomQuestionnaire     :: Questionnaire
symptomQuestionnaire     = M.fromList $ inactiveSymptomQuestions ++ activeSymptomQuestions
