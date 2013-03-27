{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad       (msum)
import Happstack.Server -- (Response, ServerPart, dir, mapServerPartT, look, nullConf, ok, simpleHTTP, toResponse, path)

import qualified Data.UUID as U
import Templates

main :: IO ()
main = simpleHTTP nullConf $ msum [ method GET >> nullDir >> (ok $ toResponse $ index U.nil)
                                  , dir "images"      $ serveDirectory DisableBrowsing [] "../static/images"
                                  , dir "stylesheets" $ serveDirectory DisableBrowsing [] "../static/stylesheets"
--                                , dir "symptoms"    $ path $ \uuid -> ok $ "Symptom questionnaire " ++ uuid
--                                , dir "trailers"    $ path $ \uuid -> ok $ "Trailer observation "   ++ uuid
                                  ]
