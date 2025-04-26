-module(pollution).
-author("bartol").

%% API
-export([create_monitor/0, add_station/3, add_value/5, remove_value/4, get_one_value/4,
    get_station_min/3, get_daily_mean/3, get_closest_stations/2]).

-record(station, {coordinates, measurements = []}).
-record(measurement,  {date, time, type, value}).



create_monitor() ->
    #{}.



cord_exists(Coordinates, M) ->
    Res = maps:filter(fun(_, V) -> V#station.coordinates == Coordinates end, M),
    if
        Res == #{} -> false;
        true -> true
    end.

station_exists(Name, Coordinates, M) ->
    case maps:is_key(Name, M) of
        true -> true;
        false -> cord_exists(Coordinates, M)
    end.

add_station(Name, Coordinates, M) ->
    case station_exists(Name, Coordinates, M) of
        true -> {error, "Station cannot be added"};
        false -> M#{Name => #station{coordinates = Coordinates}}
    end.



find_station_by_cord(Coordinates, M) ->
    Res = maps:filter(fun(_, V) -> V#station.coordinates == Coordinates end, M),
    [H | _] = maps:keys(Res),
    H.

add_meas_to_station(Station, Name, Datet, Type, Value, M) ->
    {Date, Time} = Datet,
    Measurements = Station#station.measurements,
    Filter_meas = lists:filter(fun(Elem) -> Elem#measurement.date == Date andalso Elem#measurement.time == Time andalso Elem#measurement.type == Type end, Measurements),
    case Filter_meas of
        [] -> New_Measurements = Measurements ++ [#measurement{date = Date, time = Time, type = Type, value = Value}],
            New_Station = Station#station{measurements = New_Measurements},
            M#{Name => New_Station};
        _ -> {error, "Value cannot be added"}
    end.

add_value({X, Y}, Datet, Type, Value, M) ->
    case cord_exists({X, Y}, M) of
        true -> add_value(find_station_by_cord({X, Y}, M), Datet, Type, Value, M);
        false -> {error, "Station not exists"}
    end;
add_value(Name, Datet, Type, Value, M) ->
    Find = maps:find(Name, M),
    case Find of
        error -> {error, "Station not exists"};
        {_, Station} -> add_meas_to_station(Station, Name, Datet, Type, Value, M)
    end.



remove_meas_from_station(Station, Name, Datet, Type, M) ->
    {Date, Time} = Datet,
    Measurements = Station#station.measurements,
    Filter_meas = lists:filter(fun(Elem) -> Elem#measurement.date == Date andalso Elem#measurement.time == Time andalso Elem#measurement.type == Type end, Measurements),
    case Filter_meas of
        [] -> {error, "Value cannot be removed"};
        _ -> New_Measurements = Measurements -- Filter_meas,
            New_Station = Station#station{measurements = New_Measurements},
            M#{Name => New_Station}
    end.

remove_value({X, Y}, Datet, Type, M) ->
    case cord_exists({X, Y}, M) of
        true -> remove_value(find_station_by_cord({X, Y}, M), Datet, Type, M);
        false -> {error, "Station not exists"}
    end;
remove_value(Name, Datet, Type, M) ->
    Find = maps:find(Name, M),
    case Find of
        error -> {error, "Station not exists"};
        {_, Station} -> remove_meas_from_station(Station, Name, Datet, Type, M)
    end.



get_val_from_station(Station, Datet, Type) ->
    {Date, Time} = Datet,
    Measurements = Station#station.measurements,
    Filter_meas = lists:filter(fun(Elem) -> Elem#measurement.date == Date andalso Elem#measurement.time == Time andalso Elem#measurement.type == Type end, Measurements),
    case Filter_meas of
        [] -> {error, "Value not exist"};
        [H | _] -> H#measurement.value
    end.

get_one_value({X, Y}, Datet, Type, M) ->
    case cord_exists({X, Y}, M) of
        true -> get_one_value(find_station_by_cord({X, Y}, M), Datet, Type, M);
        false -> {error, "Station not exists"}
    end;
get_one_value(Name, Datet, Type, M) ->
    Find = maps:find(Name, M),
    case Find of
        error -> {error, "Station not exists"};
        {_, Station} -> get_val_from_station(Station, Datet, Type)
    end.



get_station_min_value(Station, Type) ->
    Measurements = Station#station.measurements,
    Filter_meas = lists:filter(fun(Elem) -> Elem#measurement.type == Type end, Measurements),
    case Filter_meas of
        [] -> {error, "Value not exist"};
        _ -> lists:foldl(fun(X, A) -> min(X#measurement.value, A) end, 10000000, Filter_meas)
    end.

get_station_min({X, Y}, Type, M) ->
    case cord_exists({X, Y}, M) of
        true -> get_station_min(find_station_by_cord({X, Y}, M), Type, M);
        false -> {error, "Station not exists"}
    end;
get_station_min(Name, Type, M) ->
    Find = maps:find(Name, M),
    case Find of
        error -> {error, "Station not exists"};
        {_, Station} -> get_station_min_value(Station, Type)
    end.



get_daily_mean(Type, Date, M) ->
    Measurements = maps:fold(fun(_, V, A) -> A ++ V#station.measurements end, [], M),
    Filter_meas = lists:filter(fun(Elem) -> Elem#measurement.date == Date andalso Elem#measurement.type == Type end, Measurements),
    Mean = lists:foldl(fun(X, {Sum, Cnt}) -> {Sum + X#measurement.value, Cnt + 1} end, {0, 0}, Filter_meas),
    {Sum, Cnt} = Mean,
    if
        Cnt == 0 -> {error, "No data for given date"};
        true -> Sum/Cnt
    end.



distance(X1, Y1, X2, Y2) ->
    X = math:pow((X1 - X2), 2),
    Y = math:pow((Y1 - Y2), 2),
    math:sqrt(X + Y).

get_closest_stations({X, Y}, M) ->
    Distance = maps:map(fun(_, V) -> distance(X, Y, element(1, V#station.coordinates), element(2, V#station.coordinates)) end, M),
    if
        Distance == #{} -> {error, "Empty monitor"};
        true -> {_, Res} = maps:fold(fun(K, Val, {Min, Names}) -> if Val < Min -> {Val, [K]}; Val == Min -> {Min, [K] ++ Names}; true -> {Min, Names} end end, {1000000000, []}, Distance),
            Res
    end;
get_closest_stations(Name, M) ->
    Find = maps:find(Name, M),
    case Find of
        error -> {error, "Station not exists"};
        {_, Station} -> get_closest_stations(Station#station.coordinates, maps:without([Name], M))
    end.
