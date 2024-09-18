# fp-2024

## Setup

### To get started, you first need to open the project using Visual Studio Code and having Docker Desktop
1. `Ctrl + Shift + P`
2. `Dev Containers: Open Folder in Container`

### To Build & Test the Project, run the following commands
1. `stack build`
2. `stack test`

## Domain - Public transport and routes
**Main entities:**
- Busses
- Trains
- Bus routes
- Train routes
- Public paths

**Main operations:**
- Add/Remove bus
- Add/Remove train
- Add/Remove bus/train stop
- Create a route
- Assign bus/train to routes
- Remove bus/train from routes
- Add bus/train stop to route
- Create a public path (a path from one stop to other stop)
- Remove public path

- Print all the routes
- Print all the busses/trains
- Print all the route stops
- Print all the route busses/trains
- Join two routes at the intersecting stop (creates a new route)
- Join two routes at the provided stop (creates a new route)

Yes the same route will be able to have both busses and trains operating.
