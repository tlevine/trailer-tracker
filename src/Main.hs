{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad       (msum)
import Happstack.Server -- (Response, ServerPart, dir, mapServerPartT, look, nullConf, ok, simpleHTTP, toResponse, path)

import qualified Data.UUID as U
import Templates

devConf :: Conf
devConf = Conf { port      = 1418
               , validator  = Nothing
               , logAccess = Just logMAccess
               , timeout = 30
               , threadGroup = Nothing
               }

main :: IO ()
main = simpleHTTP devConf $ msum [ method GET >> nullDir >> (ok $ toResponse $ index U.nil)
                                  , dir "images"      $ serveDirectory DisableBrowsing [] "../static/images"
                                  , dir "stylesheets" $ serveDirectory DisableBrowsing [] "../static/stylesheets"
                                  , dir "me" $ ok $ toResponse $ placeholder "Information like your email address and phone number goes here."
                                  , dir "login" $ ok $ toResponse $ placeholder "A log-in form goes here."
                                  , dir "logout" $ ok $ toResponse $ placeholder "You have logged out."
                                  , dir "symptoms"    $ path $ \uuid -> ok $ toResponse $ placeholder $ "Symptom questionnaire " ++ uuid
                                  , dir "trailers"    $ path $ \uuid -> ok $ toResponse $ placeholder $ "Trailer observation "   ++ uuid
                                  ]
