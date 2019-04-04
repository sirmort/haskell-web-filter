{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module ImageFilters where

import Foundation
import Yesod.Core
import Vision.Image
--import Data.Conduit
import Debug.Trace

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

toRed :: (Image i, Convertible i RGB) => i -> RGB
toRed img =
    let grey = convert img :: RGB
    in fromFunction (shape img) $ \pt ->
        let pix = grey `index` pt
            r = fromIntegral $ rgbRed pix
            g = fromIntegral $ rgbGreen pix
            b = fromIntegral $ rgbBlue pix
            tr = red r g b
            tg = 0
            tb = 0
            pixel = RGBPixel { rgbRed = tr, rgbGreen = tg, rgbBlue = tb } in
        pixel
        where
			red :: Int -> Int -> Int -> Word8
			red r g b = intToWord8 $ round $ (1 * toRational r + 0 * toRational g + 0 * toRational b)
			
toGreen :: (Image i, Convertible i RGB) => i -> RGB
toGreen img =
    let grey = convert img :: RGB
    in fromFunction (shape img) $ \pt ->
        let pix = grey `index` pt
            r = fromIntegral $ rgbRed pix
            g = fromIntegral $ rgbGreen pix
            b = fromIntegral $ rgbBlue pix
            tr = 0
            tg = green r g b
            tb = 0
            pixel = RGBPixel { rgbRed = tr, rgbGreen = tg, rgbBlue = tb } in
        pixel
        where
			green :: Int -> Int -> Int -> Word8
			green r g b = intToWord8 $ round $ (0 * toRational r + 1 * toRational g + 0 * toRational b)
			
toBlue :: (Image i, Convertible i RGB) => i -> RGB
toBlue img =
    let grey = convert img :: RGB
    in fromFunction (shape img) $ \pt ->
        let pix = grey `index` pt
            r = fromIntegral $ rgbRed pix
            g = fromIntegral $ rgbGreen pix
            b = fromIntegral $ rgbBlue pix
            tr = 0
            tg = 0
            tb = blue r g b
            pixel = RGBPixel { rgbRed = tr, rgbGreen = tg, rgbBlue = tb } in
        pixel
        where
			blue :: Int -> Int -> Int -> Word8
			blue r g b = intToWord8 $ round $ (0 * toRational r + 0 * toRational g + 1 * toRational b)
            

toBlackWhite :: (Image i, Convertible i RGB) => i -> RGB
toBlackWhite img =
    let grey = convert img :: RGB
    in fromFunction (shape img) $ \pt ->
        let pix = grey `index` pt
            r = fromIntegral $ rgbRed pix
            g = fromIntegral $ rgbGreen pix
            b = fromIntegral $ rgbBlue pix
            tr = intToWord8 $ round $ min ((toRational r + toRational g + toRational b)/3) 255
            tg = intToWord8 $ round $ min ((toRational r + toRational g + toRational b)/3) 255
            tb = intToWord8 $ round $ min ((toRational r + toRational g + toRational b)/3) 255
            pixel = RGBPixel { rgbRed = tr, rgbGreen = tg, rgbBlue = tb } in
        pixel 

toInversia :: (Image i, Convertible i RGB) => i -> RGB
toInversia img =
    let grey = convert img :: RGB
    in fromFunction (shape img) $ \pt ->
        let pix = grey `index` pt
            r = fromIntegral $ rgbRed pix
            g = fromIntegral $ rgbGreen pix
            b = fromIntegral $ rgbBlue pix
            tr = intToWord8 $ round $ min (255-toRational r) 255 
            tg = intToWord8 $ round $ min (255-toRational g) 255 
            tb = intToWord8 $ round $ min (255-toRational b) 255
            pixel = RGBPixel { rgbRed = tr, rgbGreen = tg, rgbBlue = tb } in
        pixel

toSepia :: (Image i, Convertible i RGB) => i -> RGB
toSepia img =
    let grey = convert img :: RGB
    in fromFunction (shape img) $ \pt ->
        let pix = grey `index` pt
            r = fromIntegral $ rgbRed pix
            g = fromIntegral $ rgbGreen pix
            b = fromIntegral $ rgbBlue pix
            tr = intToWord8 $ round $ min (0.393 * toRational r + 0.769 * toRational g + 0.189 * toRational b) 255
            tg = intToWord8 $ round $ min (0.349 * toRational r + 0.686 * toRational g + 0.168 * toRational b) 255
            tb = intToWord8 $ round $ min (0.272 * toRational r + 0.534 * toRational g + 0.131 * toRational b) 255
            pixel = RGBPixel { rgbRed = tr, rgbGreen = tg, rgbBlue = tb } in
        pixel

intToWord8 :: Int -> Word8
intToWord8 i = fromInteger $ fromIntegral i