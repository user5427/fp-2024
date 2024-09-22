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
    - Remove trip
    - **Join two trips to create a new trip**
    - Check if trip is valid (it is all connected)
    - Check trip distance

Yes the same route will be able to have both busses and trains operating.

**Examples: (checked using BNF Playground)**

5. **Add a stop**  
```
add_bus_stop 301 Downtown 40.7128 -74.0060
```

6. **Remove a stop**  
```
remove_bus_stop 301
```

9. **Create a route**  
```
create_route 501 City_Center_Loop
```
10. **Assign a bus to a route**  
 ```
 assign_bus_to_route 101 501
 ```

11. **Assign a train to a route**  
 ```
 assign_train_to_route 201 501
 ```

12. **Add a bus stop to a route**  
 ```
 add_bus_stop_to_route 301 501
 ```

13. **Remove a bus from a route**  
 ```
 remove_bus_from_route 101 501
 ```

14. **Join two routes at the intersecting stop (creates a new route)**  
 ```
 join_two_routes 501 502 601 Combined_City_Route
 ```

15. **Join two routes at the provided stop (creates a new route)**  
 ```
 join_two_routes_at_stop 501 502 301 601 Union_Route
 ```

16. **Create a public path between two stops**  
 ```
 create_path 701 Scenic_Path 11 301 401
 ```

17. **Remove a public path**  
 ```
 remove_path 701
 ```

18. **Print all the routes**  
 ```
 print_all_routes
 ```

19. **Print all the busses**  
 ```
 print_all_busses
 ```

20. **Print all route busses**  
 ```
 print_all_route_busses 501
 ```