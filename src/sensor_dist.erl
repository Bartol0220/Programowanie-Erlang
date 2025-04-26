-module(sensor_dist).
-author("Admin").

%% API
-export([measure/0, find_for_person/3]).

get_rand_locations(N) ->
    [{rand:uniform(10000), rand:uniform(10000)} || _<-lists:seq(1, N)].

dist({X1, Y1}, {X2, Y2}) ->
    X = math:pow((X1 - X2), 2),
    Y = math:pow((Y1 - Y2), 2),
    math:sqrt(X + Y).

find_for_person(PersonLocation, SensorLocation) ->
    lists:min( [{dist(PersonLocation, SL), {PersonLocation, SL}} || SL <- SensorLocation] ).

find_closests(PeopleLocations, SensorLocations) ->
    lists:min( [find_for_person(PL, SensorLocations) || PL <- PeopleLocations] ).

measure() ->
    PeopleLocations = get_rand_locations(50000),
    SensorLocations = get_rand_locations(2000),
    io:format("Poszukiwanie~n"),
    {T1, R1} = timer:tc(fun find_closests/2, [PeopleLocations, SensorLocations]),
    io:format("Poszukiwanie równoległe~n"),
    {T2, R2} = timer:tc(fun find_closests_r/2, [PeopleLocations, SensorLocations]),
    io:format("~w - czas poszukiwania~n", [T1]),
    io:format("~w - czas poszukiwania równoległego~n", [T2]),
    R1 == R2.

find_for_person(PersonLocation, SensorLocation, PPID) ->
    PPID ! lists:min( [{dist(PersonLocation, SL), {PersonLocation, SL}} || SL <- SensorLocation] ).

find_closests_r(PeopleLocations, SensorLocations) ->
    PIDs = [spawn(?MODULE, find_for_person, [PL, SensorLocations, self()]) || PL <- PeopleLocations],
    lists:min([receive N -> N end || _ <- PIDs]).

