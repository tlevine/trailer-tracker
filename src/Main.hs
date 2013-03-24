{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module Main (main) where

import Data.Acid

import Control.Monad.State                   ( get, put )
import Control.Monad.Reader                  ( ask )
import Control.Applicative                   ( (<$>) )
import System.Environment                    ( getArgs )
import Data.SafeCopy


import qualified Data.Map as M

-- This enum comes from a list of active questions and a list of inactive questions.
type QuestionCode 

-- Regex types
-- http://ww2.cs.mu.oz.au/~sulzmann/singhaskell07/kenny-xhaskell.pdf
type Email = String
type Phone = String
type UUID  = String
type VIN = String

-- Other special types
type Answer k = (DateTime, k)

-- http://stackoverflow.com/questions/8510570/restricting-values-in-type-constructors
type Month = January | February | March | April | May | June | July | August | September | October | November | December deriving (Enum, Show)
type YearMonth = (Integer, Month)

-- User table
data Me = Me
    { id :: Integer,
      email :: Email,
      phone :: Phone,
      observations :: [UUID],
      answers :: M.Map QuestionCode Answer
    } deriving (Show,Ord,Eq,Typeable)
makeLenses ''Me

-- Trailer observation table
data Observation = Observation
    { id :: UUID,
      vin :: VIN,
      mine :: Bool,
      from :: YearMonth,
      to :: YearMonth
    }
makeLenses ''Observation

-- Save
data MeTable = MeTable [Message]
data ObservationTable = ObservationTable [Observationssage]
$(deriveSafeCopy 0 'base ''MeTable)
$(deriveSafeCopy 0 'base ''ObservationTable)

-- Transactions are defined to run in either the 'Update' monad
-- or the 'Query' monad.                                                                                                                                    
addMessage :: Message -> Update Database ()
addMessage msg
    = do Database messages <- get
         put $ Database (msg:messages)

viewMessages :: Int -> Query Database [Message]
viewMessages limit
    = do Database messages <- ask
         return $ take limit messages

-- This will define @ViewMessage@ and @AddMessage@ for us.
$(makeAcidic ''Database ['addMessage, 'viewMessages])

main :: IO ()
main = do args <- getArgs
          database <- openLocalStateFrom "myDatabase/" (Database ["Welcome to the acid-state database."])
          if null args
            then do messages <- query database (ViewMessages 10)
                    putStrLn "Last 10 messages:"
                    mapM_ putStrLn [ "  " ++ message | message <- messages ]
            else do update database (AddMessage (unwords args))
                    putStrLn "Your message has been added to the database."

