### Lab 1

Program aim - check if given trip is valid (It is all connected)

**Main entities:**
- Stops
- Routes
- Public paths
- Trips

**Main operations:**
- Stops and routes
    - Create stop
    - Create a route

- Public paths
    - Create a public path (a path from one stop to other stop)

- Joining routes
    - Join two routes at the intersecting stop (creates a new route)
    - Join two routes at the provided stop (creates a new route)

- Trips
    - Create a trip
    - Join two trips to create a new trip
    - Trip validation
    - Trip cleanup (creates a new route)
    - Trip distance

### Examples:

1. 
```
validate_trip(create_trip(68, Name, create_stop(41, BusStopA, 7691.96, 0.1, 67, 2),create_stop(3, BusStopB, -4, 0, 80, 2),7,create_stop(9, BusStopC, -6, 0, 3, 7)), 5, 4)
```

2.
```
join_two_trips(create_trip(40, Name, 0,create_stop(3, Stop, -96.9, -0, 2, 3)), 2, 1, NewTrip)
```

3.
```
join_two_routes(create_route(4, D_Route, 067,8), join_two_routes(join_two_routes_at_stop(create_route(3, C_Route, 11), join_two_routes_at_stop(join_two_routes(52, 0, 6, F_Route), 6, 3, 9, BB_Route), 8, 577, AA_Route), 49, 70, H_Route), 9, G_Route)
```

4.
```
join_two_trips(create_trip(7, Trip_Name, 3,create_stop(62, Name, 0, 75.5, 19537, 530),create_stop(4, Name, -0, 1, 94, 50)), 75, 4, New_Name)
```

5.
```
join_two_routes(join_two_routes(join_two_routes(join_two_routes(52, 0, 6, F_Route), join_two_routes(52, 0, 6, F_Route), 6, F_Route), 0, 6, F_Route), 0, 6, F_Route)
```
[BNF FILE](BNF.txt)

<!-- 
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
``` -->

<!-- ### Examples of simple commands: (checked using BNF Playground)

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
 ``` -->
