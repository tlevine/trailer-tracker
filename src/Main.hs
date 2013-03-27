module Main where

import Control.Monad
import Happstack.Server    (Response, ServerPart, dir, mapServerPartT, look, nullConf, ok, simpleHTTP, toResponse, path)

import qualified Data.UUID as U
import Templates

{-
mai1 :: IO ()
mai1 = simpleHTTP nullConf $ msum [ dir "trailers"    $ path $ \uuid -> ok "Trailer observation "   ++ uuid
                                  , dir "symptoms"    $ path $ \uuid -> ok "Symptom questionnaire " ++ uuid
--                                , dir "images"      $ serveDirectory "static/images"
--                                , dir "stylesheets" $ serveDirectory "static/stylesheets"
                                  ]
-}

simpleIndex :: ServerPart Response
simpleIndex = ok $ toResponse $ index U.nil

main :: IO ()
main = simpleHTTP nullConf $ simpleIndex
