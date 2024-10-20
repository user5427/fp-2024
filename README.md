# fp-2024

## Setup

### To get started, you first need to open the project using Visual Studio Code and having Docker Desktop
1. `Ctrl + Shift + P`
2. `Dev Containers: Open Folder in Container`

### To Build & Test the Project, run the following commands
1. `stack build`
2. `stack test`

## Domain - Public transport and routes

[lab1](lab1.md)


### Lab Two

BNF file changes:
*  cleanup_trip, validate_trip, find_next_stop, find_previous_stop functions require less arguments or different arguments now.
*  added set_next_stop and set_previous_stop functions because one stop can have multiple routes with different next or previous stops.
* removed `<neg>` and replaced it with just `"-" <pos>` inside the `<float>` 
* removed `<trip_name>` and `<route_name>`. Replaced with `<name>`. Reason: not needed.
* ~~removed `<new_trip_id>` and `<new_route_id>` and replaced with `<trip_id>` and `<route_id>`. Reason: for better clarity.~~ Readded. Reason: for better clarity in BNF. Not recreated in `Lib2.hs`
* added `<point>` which includes `<coord_x>` and `<coord_y>`
* `<next_stop_id> ::= <stop_id>`, `<previous_stop_id> ::= <stop_id>` before stop_id it was integer
* added func (for debug or other): assign_stop_to_route, distance_betweem_stops, connect_route_stops_by_min_dist, check_if_route_stops_connected


things only in found in BNF next_stop_id, previous_stop_id, new_name, new_trip_id, new_route_id