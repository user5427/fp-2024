# fp-2024

## Setup

### To get started, you first need to open the project using Visual Studio Code and having Docker Desktop
1. `Ctrl + Shift + P`
2. `Dev Containers: Open Folder in Container`

### To Build & Test the Project, run the following commands
1. `stack build`
2. `stack test`

## Domain - Public transport and routes

Program aim - check if given trip is valid (It is all connected)

**Main entities:**
- Stops
- Routes
- Public paths
- Trips

**Main operations:**
- Stops and routes
    - Add/Remove stop
    - Create a route
    - Add stop to route
    - Add next stop id to a stop
    - Add previous stop id to a stop

- Public paths
    - Create a public path (a path from one stop to other stop)
    - Remove public path

- Printing
    - Print all the routes
    - Print all the route stops
    - Print all public paths
    - Print all trips

- Joining routes
    - **Join two routes at the intersecting stop (creates a new route)**
    - **Join two routes at the provided stop (creates a new route)**

- Trips
    - Create a trip
    - Add paths to the trip
    - Remove paths from the trip
    - Add route stops to the trip
    - Remove route stops from the trip
    - Get stops and paths
    - Remove trip
    - **Join two trips to create a new trip**
    - **Recursive trip validation**
    - **Recursive trip cleanup**
    - **Trip distance** (recursive)

### Examples of recursive commands (stop_ids changed to strings for clarity)

Diagram of the routes and paths:
stop_1 -> stop_2 -> path_1 -> stop_3.

1. **Recursive trip validation.**
Trip: stop_1 stop_2,path_1,stop_3.

```
recursive_trip_validation (123 stop_1 stop_2,path_1,stop_3) recursive_trip_validation
stop_1 AND stop_2 connected.
- recursive_trip_validation (123 stop_2 path_1,stop_3) recursive_trip_validation
  stop_2 AND path_1 connected.
-- recursive_trip_validation (123 path_1 stop_3) recursive_trip_validation
   path_1 AND stop_3 connected.
--- recursive_trip_validation (123 stop_3)
    LIST EMPTY. END.
result: true.
```

2. **Recursive trip cleanup.**
Trip: stop_2 stop_3,path_1,stop_1 -> not valid.

```
recursive_trip_cleanup (123 stop_2 stop_3,path_1,stop_1) 
find_partner_stop (stop_2 stop_3,path_1,stop_1) recursive_trip_cleanup
+ find_partner_stop (stop_2 stop_3,path_1,stop_1) find_partner_stop
  stop_2 AND stop_3 NOT CONNECTED. CONTINUE.
++ find_partner_stop(stop_2 path_1, stop_1, ) find_partner_stop
   stop_2 AND path_1 CONNECTED. END.
- recursive_trip_cleanup (123 path_1 stop_3,stop_1) 
  find_partner_stop (path_1 stop_3,stop_1) recursive_trip_cleanup
  + find_partner_stop (path_1 stop_3,stop_1) find_partner_stop
    path_1 AND stop_3 CONNECTED.END.
-- recursive_trip_cleanup (123 stop_3 stop_1) 
   find_partner_stop (stop_3 stop_1) recursive_trip_cleanup
   + find_partner_stop (stop_3 stop_1) find_partner_stop
     stop_3 AND stop_1 NOT CONNECTED. CONTINUE.
   ++ find_partner_stop (stop_3) find_partner_stop
      LIST EMPTY. END.
--- recursive_trip_cleanup (123 stop_1) 
    LIST EMPTY. INSERT stop_1 at the begining.
result: stop_1 -> stop_2 -> path_1 -> stop_3
```

3. **Find trip distance.**
Trip: stop_1 stop_2,path_1,stop_3.

```
trip_distance (123 stop_1 stop_2,path_1,stop_3) trip_distance
stop_1 AND stop_2 distance -> 10 km.
- trip_distance (123 stop_2 path_1,stop_3) trip_distance
  stop_2 AND path_1 distance -> 15 km.
-- trip_distance (123 path_1 stop_3) trip_distance
   path_1 AND stop_3 distance -> 5 km.
--- trip_distance (123 stop_3)
    LIST EMPTY. END.
result: 30 km.
```

### Examples of simple commands: (checked using BNF Playground)

1. **Add a stop**  
```
add_stop 301 Downtown 40.7128 -74.0060
```

2. **Remove a stop**  
```
remove_stop 301
```

3. **Create a route**  
```
create_route 501 City_Center_Loop
```

4. **Add a bus stop to a route**  
 ```
 add_stop_to_route 301 501
 ```

5. **Remove a bus from a route**  
 ```
 remove_from_route 101 501
 ```

6. **Join two routes at the intersecting stop (creates a new route)**  
 ```
 join_two_routes 501 502 601 Combined_City_Route
 ```

7. **Join two routes at the provided stop (creates a new route)**  
 ```
 join_two_routes_at_stop 501 502 301 601 Union_Route
 ```

8. **Create a public path between two stops**  
 ```
 create_path 701 Scenic_Path 11 301 401
 ```

9. **Remove a public path**  
 ```
 remove_path 701
 ```
