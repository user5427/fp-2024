
# fp-2024

[lab1](lab1.md)
[lab2](lab2.md)

## lab Three

### changes to lab2 code
`parseQuery :: String -> Either String Query` changed to `parseQuery :: String -> Either String (Query, String)` for command batching to work.

### grouping queries
BNF Grouping queries syntax: ```<batch> ::= "BEGIN " <command_list> " END"```
Examples
  * ```
    BEGIN create_stop(S1, Seskine, 0.55, 0.66); create_stop(S2, Gelvoneliu, 1.55, 1.66); END
  * ```
    BEGIN create_route(R1, imabouttodiefromhaskell, S1, S2, S3, S4); connect_route_stops_by_min_dist(R1); END
    ```

### :paste
```
-- Entering multi-line mode. Press <Ctrl-D> to finish.
| BEGIN
| create_stop(S1, Seskine, 0.55, 0.66);
| create_stop(S2, Gelvoneliu, 1.55, 1.66);
| create_stop(S3, Gelvonu, 2.55, 2.66);
| create_stop(S4, Jovaro, 3.55, 3.66);
| END
| 
[0] Stop Name "Seskine" created
[1] Stop Name "Gelvoneliu" created
[2] Stop Name "Gelvonu" created
[3] Stop Name "Jovaro" created
```

### SAVE
state.txt
```
BEGIN create_stop(S1, Seskine, 0.55, 0.66); 
create_stop(S2, Gelvoneliu, 1.55, 1.66); 
create_stop(S3, Gelvonu, 2.55, 2.66); 
create_stop(S4, Jovaro, 3.55, 3.66); 
create_route(R1, imabouttodiefromhaskell, S1, S2, S3, S4); 
set_next_stop(S1, R1, S2); 
set_next_stop(S2, R1, S3); 
set_previous_stop(S2, R1, S1); 
set_next_stop(S3, R1, S4); 
set_previous_stop(S3, R1, S2); 
set_previous_stop(S4, R1, S3); 
END
```

[lab3_example](src/Lib3/lab3_example.txt)


