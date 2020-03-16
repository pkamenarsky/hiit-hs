{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TupleSections #-}

module Lib where

import Concur.Core (Widget, orr, display)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative
import Control.Concurrent
import Concur.Replica (Attr(..), Attrs, HTML, VDOM(..), clientDriver, div, text)
import Concur.Replica as Replica
import qualified Data.Map as M
import Data.Maybe
import Data.Text.Encoding (decodeUtf8)
import Data.Text (Text, intercalate, pack)
import Replica.VDOM (HTML)

import Network.WebSockets (defaultConnectionOptions)
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Static as Static

import GHC.Generics

import Network.Wai.Handler.Replica as R

import Prelude hiding (cycle, div, span)

import Debug.Trace

--------------------------------------------------------------------------------

someFunc :: IO ()
someFunc = Lib.run 8080

run :: Int -> IO ()
run port = do
  Replica.run
    port
    index
    defaultConnectionOptions
    static
    hiit

static :: Wai.Middleware
static = Static.staticPolicy $ Static.only
  [ ("style.css", "style.css")
  , ("fonts/Muller-ExtraBold-DEMO.ttf", "fonts/Muller-ExtraBold-DEMO.ttf")
  ]

index :: HTML
index =
  [ VLeaf "!doctype" (fl [("html", ABool True)]) Nothing
  , VNode "html" mempty Nothing
      [ VNode "head" mempty Nothing
          [ VLeaf "meta" (fl [("charset", AText "utf-8")]) Nothing
          , VLeaf "meta" (fl [("viewport", AText "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0")]) Nothing

          , VNode "title" mempty Nothing [VText "HIIT"]

          , VLeaf "meta"
              (fl [ ("name", AText "viewport")
                  , ("content", AText "width=device-width, initial-scale=1")
                  ]
              )
              Nothing

          , VLeaf "link"
              (fl [ ("href", AText "style.css")
                  , ("rel", AText "stylesheet")
                  ]
              )
              Nothing
          ]

      , VNode "body" mempty Nothing
          [ VNode "script" (fl [("language", AText "javascript")]) Nothing
              [ VRawText $ decodeUtf8 clientDriver ]
          ]
      ]
  ]
  where
    fl :: [(Text, Replica.Attr)] -> Replica.Attrs
    fl = M.fromList

--------------------------------------------------------------------------------

newKeypressChan :: Context -> IO (Chan Int)
newKeypressChan ctx = do
  chan <- newChan
  cb <- registerCallback ctx $ \key -> writeChan chan key
  call ctx cb "window.onkeydown = function(event) { callCallback(arg, event.keyCode) };"
  pure chan

timer :: Int -> Widget HTML ()
timer x
  | x > 0 = do
      div [ className "timer" ] [ text (pack $ show x), liftIO (threadDelay 1000000) ]
      timer (x - 1)
  | otherwise = pure ()

data Cycle = Cycle
  { duration :: Maybe Int
  , mode :: Text
  , set :: Int
  }

cycle :: Cycle -> Widget HTML ()
cycle cycle = div [ className "container" ]
  [ case duration cycle of
      Just t -> timer t
      Nothing -> display mempty

  , div [ className "mode" ] [ text (mode cycle) ]
  , div [ className "cycle" ] [ text (intercalate " " $ replicate (set cycle) "●") ]
  ]

hiit :: Context -> Widget HTML ()
hiit ctx = do
  ch <- liftIO $ newKeypressChan ctx

  skippable ch
    [ (Nothing, cycle Cycle { duration = Just 5, mode = "SET 1 - JUMP", set = 1 })
    , (Nothing, cycle Cycle { duration = Just 3, mode = "SET 1 - REST", set = 1 })

    , (Nothing, cycle Cycle { duration = Just 5, mode = "SET 1 - JUMP", set = 2 })
    , (Nothing, cycle Cycle { duration = Just 3, mode = "SET 1 - REST", set = 2 })

    -- p
    , (Just 80, cycle Cycle { duration = Just 3, mode = "PAUSE", set = 0 })

    , (Nothing, cycle Cycle { duration = Just 5, mode = "SET 2 - JUMP", set = 1 })
    , (Nothing, cycle Cycle { duration = Just 3, mode = "SET 2 - REST", set = 1 })

    , (Just 80, cycle Cycle { duration = Nothing, mode = "WELL DONE!", set = 0 })
    ]
  where
    waitForCode ch ws = do
      code <- liftIO $ readChan ch
      case dropWhile ((Just code /=) . fst) ws of
        []  -> waitForCode ch ws
        ws' -> pure ws'

    skippable ch [] = pure ()
    skippable ch ((_, w):ws) = do
      r <- div [ fmap (const $ Right ws) onClick ]
        [ fmap Left w
        , fmap Right (waitForCode ch ws)
        ]

      case r of
        Left _ -> skippable ch ws
        Right ws' -> skippable ch ws'
