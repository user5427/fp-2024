module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = [
    "add_stop", "remove_stop", "create_route", "add_stop_to_route", 
    "remove_stop_from_route", "next_stop", "previous_stop", 
    "print_all_routes", "print_all_trips", "print_all_paths", 
    "join_two_routes", "join_two_routes_at_stop", "join_two_trips", 
    "create_path", "remove_path", "create_trip", "add_paths_to_trip", 
    "remove_paths_from_trip", "add_stops_to_trip", "remove_stops_from_trip", 
    "remove_trip", "recursive_trip_validation", "recursive_trip_cleanup", 
    "trip_distance", "get_stops_and_paths"
]