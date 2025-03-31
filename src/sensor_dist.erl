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
%%    PeopleLocations = get_rand_locations(20000),
%%    SensorLocations = get_rand_locations(1000),
    PeopleLocations = get_rand_locations(200),
    SensorLocations = get_rand_locations(10),
    {_, _} = timer:tc(fun find_closests/2, [PeopleLocations, SensorLocations]),
%%    R1.
    find_closests_r(PeopleLocations, SensorLocations).

find_for_person(PersonLocation, SensorLocation, PPID) ->
    PPID ! lists:min( [{dist(PersonLocation, SL), {PersonLocation, SL}} || SL <- SensorLocation] ).

find_closests_r(PeopleLocations, SensorLocations) ->
    PIDs = [spawn(?MODULE, find_for_person/3, [PL, SensorLocations, self()]) || PL <- PeopleLocations],
    lists:min([receive N -> N end || _ <- PIDs]).

