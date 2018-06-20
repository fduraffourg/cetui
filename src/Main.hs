{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Main where

import qualified CE.Client as CE
import qualified CE.Models as M
import qualified UI.DomainChooser as DC
import UI.Bookings

main :: IO ()
main = do
    _ <- putStrLn "Fetching sites from Core Engine"
    result <- CE.getSites
    case result of
        Just sites ->
            do
                mSite <- DC.letUserSelectSite sites
                case mSite of
                    Just site -> do
                        let M.Site siteID _ _ = site
                        extent <- CE.getSiteExtent siteID
                        case extent of
                            Just extent -> do
                                _ <- runBookingsUI site extent
                                return ()
                            Nothing -> do
                                putStrLn "Failed to fetch extent"
                    Nothing -> putStrLn "No site selected"
        Nothing -> putStrLn "Failed to fetch sites"
