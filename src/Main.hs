{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Main where

import qualified CE.Client as CE
import qualified UI.DomainChooser as DC

main :: IO ()
main = do
    _ <- putStrLn "Fetching sites from Core Engine"
    result <- CE.getSites
    case result of
        Just sites ->
            do
                _ <- putStrLn "Done fetching sites"
                site <- DC.letUserSelectSite sites
                _ <- putStrLn "Choosen site"
                _ <- putStrLn $ show site
                return ()
        Nothing -> putStrLn "Failed to fetch sites"
