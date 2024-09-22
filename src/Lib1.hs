module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = [
    "create_route", "join_two_routes", "join_two_routes_at_stop", 
    "join_two_trips", "create_path", "create_trip", 
    "validate_trip", "cleanup_trip", "trip_distance"]