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
