{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Main where

import CE.Client
import CE.Models
import qualified UI.DomainChooser as DC
import UI.Bookings
import qualified Data.Text as T

siteID = SiteID (T.pack "a5493aa7-81f3-42e9-bd8f-335edf5319de")
site = Site siteID (T.pack "Paris") (T.pack "paris")
extent = Extent (Coordinate 46.231117248535156 7.357924461364746) (Coordinate 46.23457717895508 7.3666839599609375)

main :: IO ()
main = do
    _ <- putStrLn "Fetching sites from Core Engine"
    result <- sendBooking site extent
    return ()
