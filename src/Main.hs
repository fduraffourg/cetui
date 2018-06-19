{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Main where

import qualified CE.Client as CE
import qualified UI.DomainChooser as DC
import UI.Bookings

main :: IO ()
main = do
    _ <- putStrLn "Fetching sites from Core Engine"
    result <- CE.getSites
    case result of
        Just sites ->
            do
                _ <- putStrLn "Done fetching sites"
                _ <- runBookingsUI $ head sites
                return ()
        Nothing -> putStrLn "Failed to fetch sites"
