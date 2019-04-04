{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Home where

import Foundation
import Yesod.Core
import Yesod.Form.Input
import Yesod.Form.Fields
--import Data.Conduit
import Debug.Trace

import Vision.Image
import qualified Vision.Image.JuicyPixels   as Friday
import qualified Vision.Primitive           as Friday
import qualified Codec.Picture              as JP
import qualified Codec.Picture.Types        as JP

import qualified System.FilePath            as FilePath
import qualified Data.Text                  as Text

import Control.Monad.Trans.Resource (runResourceT)

import Data.List
import Data.Word8
import Data.Bits

import ImageFilters

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Minimal Multifile"
    [whamlet|
            <script src=https://code.jquery.com/jquery-3.3.1.min.js>
            <form id=picForm method=post action=@{FilterR} enctype=multipart/form-data>
                <p>Picture:
                <input type=file name=picture>
                <p>Filter:
                <p><input type=radio name=filter value=sepia>Sepia
                <p><input type=radio name=filter value=blackwhite>Black White
                <p><input type=radio name=filter value=inversia>Inversia
                <p><input type=radio name=filter value=red>Red
                <p><input type=radio name=filter value=green>Green
                <p><input type=radio name=filter value=blue>Blue
                <br>
                <input type=submit>
    |]

postFilterR :: Handler Html
postFilterR = defaultLayout $ do
    setTitle "PhotoFilters"
    imgFileInfo <- runInputPost $ ireq fileField "picture"
    imgFileName <- return(let fn = fileName imgFileInfo in trace (show fn) fn)
    selFilt <- runInputPost $ ireq textField "filter"
    liftIO $ fileMove imgFileInfo "tempimg"
    liftIO $ processImage $ Text.unpack selFilt
    sendFile "image" "tempimg"

processImage :: String -> IO ()
processImage selFilt = do
    io <- JP.readImage "tempimg"
    fridayRGB <- return $
        case io of
            Right (JP.ImageRGB8 img) -> do
                Friday.toFridayRGB img
            Right (JP.ImageYCbCr8 img) -> do
                Friday.toFridayRGB $ JP.convertImage img
    processed <- return $
        case selFilt of
            "sepia" -> toSepia fridayRGB
            "blackwhite" -> toBlackWhite fridayRGB
            "inversia" -> toInversia fridayRGB
            "red" -> toRed fridayRGB
            "green" -> toGreen fridayRGB
            "blue" -> toBlue fridayRGB
    --toSepia fridayRGB
    JP.savePngImage "tempimg" $ JP.ImageRGB8 $ Friday.toJuicyRGB $ processed
    putStrLn "Success"