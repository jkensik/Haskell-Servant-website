{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExtendedDefaultRules #-}


module Main where


import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Text hiding (map)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html
import Lucid
import Lucid.Base
import Servant.HTML.Lucid
import Servant
import Servant.Server



main :: IO ()
main = run 80 app

type API = Raw

htmlAPI :: Proxy API
htmlAPI = Proxy

htmlServer :: Server API
htmlServer =  serveDirectoryFileServer "../html/website"


app :: Application
app = serve htmlAPI htmlServer


html :: Html ()
html = table_ (tr_ (td_ "this is a test") <> (td_ "My table."))


example2 :: Html ()
example2 = html_ do
  head_ do
    title_ "HTML from Haskell"
    link_ [rel_ "stylesheet", type_ "text/css", href_ "https://bootswatch.com/4/journal/bootstrap.min.css"]
  body_ do
    p_ "Generating HTMl form Haskell datatypes:"
    ul_ $ mapM_ (li_ . toHtml . show) [1 .. 100]


--website :: Html ()
--website = html_ do
--  head_ do
--    title_ "Jack Kensik's website"
--    link_ [rel_ "stylesheet", href_ "https://bootswatch.com/4/journal/bootstrap.min.css"]
--  body_ do
--    nav_ [classes_ ["navbar" , "navbar-light" , "bg-light"]]
--         (a_ [class_ "navbar-brand"] "Navbar")
    
    
    
    
      
      
      
    
    
      
