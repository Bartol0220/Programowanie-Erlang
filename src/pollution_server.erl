-module(pollution_server).
-author("bartol").

%% API
-export([start/0, stop/0, init/0, serv_loop/1, get_monitor/0, add_station/2, add_value/4, remove_value/3, get_one_value/3, get_station_min/2, get_daily_mean/2, get_closest_stations/1]).



start() ->
    spawn(?MODULE, init, []).

stop() ->
    pollution_serv ! pause.

init() ->
    M = pollution:create_monitor(),
    register(pollution_serv, spawn(?MODULE, serv_loop, [M])).



serv_loop({error, _}, Old_monitor) ->
    serv_loop(Old_monitor);
serv_loop(New_monitor, _) ->
    serv_loop(New_monitor).

serv_loop(M) ->
    receive
        pause -> ok;
        {get_monitor, Pid} -> Pid ! M, serv_loop(M);
        {add_station, Pid, Name, Coordinates} -> NM = pollution:add_station(Name, Coordinates, M), Pid ! NM, serv_loop(NM, M);
        {add_value, Pid, Station, Datet, Type, Value} -> NM = pollution:add_value(Station, Datet, Type, Value, M), Pid ! NM, serv_loop(NM, M);
        {remove_value, Pid, Station, Datet, Type} -> NM = pollution:remove_value(Station, Datet, Type, M), Pid ! NM, serv_loop(NM, M);
        {get_one_value, Pid, Station, Datet, Type} -> Res = pollution:get_one_value(Station, Datet, Type, M), Pid ! Res, serv_loop(M);
        {get_station_min, Pid, Station, Type} -> Res = pollution:get_station_min(Station, Type, M), Pid ! Res, serv_loop(M);
        {get_daily_mean, Pid, Type, Date} -> Res = pollution:get_daily_mean(Type, Date, M), Pid ! Res, serv_loop(M);
        {get_closest_stations, Pid, Station} -> Res = pollution:get_closest_stations(Station, M), Pid ! Res, serv_loop(M);
        _ -> serv_loop(M)
    end.



get_monitor() ->
    pollution_serv ! {get_monitor, self()},
    receive
        M -> M
    end.



add_station(Name, Coordinates) ->
    pollution_serv ! {add_station, self(), Name, Coordinates},
    receive
        R -> R
    end.

add_value(Station, Datet, Type, Value) ->
    pollution_serv ! {add_value, self(), Station, Datet, Type, Value},
    receive
        R -> R
    end.

remove_value(Station, Datet, Type) ->
    pollution_serv ! {remove_value, self(), Station, Datet, Type},
    receive
        R -> R
    end.

get_one_value(Station, Datet, Type) ->
    pollution_serv ! {get_one_value, self(), Station, Datet, Type},
    receive
        R -> R
    end.

get_station_min(Station, Type) ->
    pollution_serv ! {get_station_min, self(), Station, Type},
    receive
        R -> R
    end.

get_daily_mean(Type, Date) ->
    pollution_serv ! {get_daily_mean, self(), Type, Date},
    receive
        R -> R
    end.

get_closest_stations(Station) ->
    pollution_serv ! {get_closest_stations, self(), Station},
    receive
        R -> R
    end.