module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = [
    "create_route", "join_two_routes", "join_two_routes_at_stop", 
    "join_two_trips", "create_path", "create_trip", 
    "validate_trip", "cleanup_trip", "trip_distance",
    "create_stop", "set_next_stop", "set_previous_stop", 
    "connect_route_stops_by_min_dist", "check_if_route_stops_connected", 
    "distance_between_stops", "assign_stop_to_route", 
    "find_next_stop", "find_previous_stop"]
