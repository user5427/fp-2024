module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = [
    "add_bus", "remove_bus", "add_train", "remove_train", "add_bus_stop",
    "remove_bus_stop", "add_train_stop", "remove_train_stop", "create_route",
    "assign_bus_to_route", "assign_train_to_route", "remove_bus_from_route",
    "remove_train_from_route", "add_bus_stop_to_route", "remove_bus_stop_from_route",
    "add_train_stop_to_route", "remove_train_stop_from_route", "print_all_routes",
    "print_all_busses", "print_all_trains", "print_all_route_busses",
    "print_all_route_trains", "join_two_routes", "join_two_routes_at_stop",
    "create_path", "remove_path", "create_trip", "add_paths_to_trip",
    "remove_paths_from_trip", "add_stops_to_trip", "remove_stops_from_trip",
    "remove_trip", "join_two_trips", "print_all_trips", "trip_valid",
    "next_stop", "previous_stop", "trip_distance"]
