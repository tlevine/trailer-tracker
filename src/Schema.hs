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
data QuestionnaireId = TrailerId U.UUID | SymptomId U.UUID
newtype GuestKey = GuestKey U.UUID
newtype UserId   = UserId   U.UUID

data User
  = OnymousUser { trailers :: S.Set TrailerId
                , symptoms :: S.Set SymptomId
                } deriving (Eq, Ord, Read, Show, Data, Typeable)
  | AnonymousUser { email :: String
                  , phone :: String
                  , trailers :: S.Set TrailerId
                  , symptoms :: S.Set SymptomId
                  } deriving (Eq, Ord, Read, Show, Data, Typeable)

-- Edits to a user: Non-nothings are changed, nothings are ignored,
-- and sets are combined.
data UserEdit = UserEdit { email :: Maybe String
                         , phone :: Maybe String
                         } deriving (Eq, Ord, Read, Show, Data, Typeable)

-- A questionnaire is a map of question codes to question answers.
type Questionnaire = M.Map String QuestionAnswer

-- Acidic types
data People = People { users :: M.Map UserId User
                     , guestKeys :: M.Map GuestKey UserId
                     } deriving (Eq, Ord, Read, Show, Data, Typeable)
type Questionnaires = M.Map QuestionnaireId Questionnaire

$(deriveSafeCopy 0 'base ''People)
$(deriveSafeCopy 0 'base ''Questionnaires)

initialPeople :: People { users = M.fromList []
                        , guestKeys = M.fromList []
}

initialGuestKeys :: GuestKeys
initialGuestKeys = M.fromList []

-----------------------------------------------------------
-- Queries
-----------------------------------------------------------

-- Make a new user.
newUser :: UserId -> User -> Update People ()
newUser userId user =
  do p@People{..} <- get
     let newUsers = M.insert userId user users
     put $ p { users = newUsers, guestKeys = guestKeys }
     return ()

-- Edit a user.
editUser :: UserId -> UserEdit -> Update People User
editUser userId userEdits =
  do

-- Look up a user's information
lookupUser :: UserId -> Query People User
lookupUser userId =
  do p@People{..} <- get
     return lookup userId users

-- Merge a guest user with an onymous user.
mergeUsers :: UserId -> GuestKey -> Update People (Maybe User)
mergeUsers onymousUserId guestKey =
  do p@People{..} <- get
     case (lookup onymousUserId users) of
       Nothing        -> return Nothing
       Just oldOnymousUser -> case (lookup guestKey guestKeys) of
         Nothing        -> return Nothing
         Just anonymousUserId -> case (lookup anonymousUserId users) of
           Nothing        -> return Nothing
           Just anonymousUser -> do
             let newOnymousUser = onymizeUser anonymousUser oldOnymousUser
             newUsers = M.delete anonymousUserId
                      $ M.adjust (\_ -> newOnymousUser) onymousUserId
             newGuestKeys = M.adjust (\_ -> onymousUserId) guestKeys

             put $ p { users = newUsers, guestKeys = newGuestKeys }
             return newOnymousUser

-- A pure function to help the above function
onymizeUser :: AnonymousUser -> OnymousUser -> User
onymizeUser = anonymousUser onymousUser =
  User { email = email onymousUser
       , phone = phone onymousUser
       , trailers = S.union (trailers onymousUser) (trailers anonymousUser)
       , symptoms = S.union (symptoms onymousUser) (symptoms anonymousUser)
  }
      
      
         

  users


-- Anonymity
lookupGuestKey :: Query People GuestKey

newGuest :: GuestKey -> Update People GuestKey


-- Questionnaire
lookupQuestionnaire :: Query Questionnaires QuestionnaireId

answerQuestionnaire :: QuestionnaireId -> Update Questionnaires QuestionnaireId




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
