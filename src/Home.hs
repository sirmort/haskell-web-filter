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

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Minimal Multifile"
    [whamlet|
        <p>
            <a href=@{AddR 5 7}>HTML addition
        <p>
            <a href=@{AddR 5 7}?_accept=application/json>JSON addition
        <p>
            <a href=@{AddR 10 7}>Haskell soset
        <p>
            <form method=post action=@{FilterR} enctype=multipart/form-data>Picture:
                <input type=file name=picture>
                <input type=submit>
        
    |]

postFilterR :: Handler Html
postFilterR = defaultLayout $ do
    setTitle "Minimal Multifile"
    imgFileInfo <- runInputPost $ ireq fileField "picture"
    --bytes <- runResourceT $ fileSource imgFileInfo $$ sinkLbs
    --liftIO $ write2server bytes "1.jpg"
    --liftIO $ Prelude.writeFile "1.jpg" Char8.unpack(bytes)
    imgFileName <- return(let fn = fileName imgFileInfo in trace (show fn) fn)
    liftIO $ fileMove imgFileInfo "tempimg"
    liftIO $ processImage
    sendFile "image" "tempimg"

processImage :: IO ()
processImage = do
    io <- JP.readImage "tempimg"
    case io of
        Left err           -> do
            putStrLn "Unable to load the image:"
            print err
        Right (JP.ImageRGB8 img) -> do
            fridayRGB <- return $ Friday.toFridayRGB img
            processed <- return $ toSepia fridayRGB
            JP.savePngImage "tempimg" $ JP.ImageRGB8 $ Friday.toJuicyRGB $ processed
            putStrLn "Success"
        Right (JP.ImageYCbCr8 img) -> do
            fridayRGB <- return $ Friday.toFridayRGB $ JP.convertImage img
            processed <- return $ toSepia fridayRGB
            JP.savePngImage "tempimg" $ JP.ImageRGB8 $ Friday.toJuicyRGB $ processed
            putStrLn "Success"

toSepia :: (Image i, Convertible i RGB) => i -> RGB
toSepia img =
    let grey = convert img :: RGB
    in fromFunction (shape img) $ \pt ->
        let pix = grey `index` pt
            r = fromIntegral $ rgbRed pix
            g = fromIntegral $ rgbGreen pix
            b = fromIntegral $ rgbBlue pix
            tr = sepiaR r g b
            tg = sepiaG r g b
            tb = sepiaB r g b
            pixel = RGBPixel { rgbRed = tr, rgbGreen = tg, rgbBlue = tb } in
        pixel
        where
            sepiaR :: Int -> Int -> Int -> Word8
            sepiaR r g b = intToWord8 $ round $ min (0.393 * toRational r + 0.769 * toRational g + 0.189 * toRational b) 255
            sepiaG :: Int -> Int -> Int -> Word8
            sepiaG r g b = intToWord8 $ round $ min (0.349 * toRational r + 0.686 * toRational g + 0.168 * toRational b) 255
            sepiaB :: Int -> Int -> Int -> Word8
            sepiaB r g b = intToWord8 $ round $ min (0.272 * toRational r + 0.534 * toRational g + 0.131 * toRational b) 255


intToWord8 :: Int -> Word8
intToWord8 i = fromInteger $ fromIntegral i