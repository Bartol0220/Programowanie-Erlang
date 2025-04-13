-module(pollution_server).
-author("bartol").

%% API
-export([start/0, stop/0, init/0, serv_loop/1, get_monitor/0, add_station/2, add_value/4, remove_value/3, get_one_value/3, get_station_min/2, get_daily_mean/2, get_closest_stations/1]).



start() ->
    register(pollution_serv, spawn(?MODULE, init, [])).

stop() ->
    pollution_serv ! pause.

init() ->
    M = pollution:create_monitor(),
    serv_loop(M).



serv_loop(M) ->
    receive
        pause -> ok;
        {new_monitor, NM} -> serv_loop(NM);
        {get_monitor, Pid} -> Pid ! M, serv_loop(M)
    end.

get_monitor() ->
    pollution_serv ! {get_monitor, self()},
    receive
        M -> M
    end.



add_station(Name, Coordinates) ->
    R = pollution:add_station(Name, Coordinates, get_monitor()),
    case R of
        {error, _} -> R;
        _ -> pollution_serv ! {new_monitor, R}, R
    end.

add_value(Station, Datet, Type, Value) ->
    R = pollution:add_value(Station, Datet, Type, Value, get_monitor()),
    case R of
        {error, _} -> R;
        _ -> pollution_serv ! {new_monitor, R}
    end.

remove_value(Station, Datet, Type) ->
    R = pollution:remove_value(Station, Datet, Type, get_monitor()),
    case R of
        {error, _} -> R;
        _ -> pollution_serv ! {new_monitor, R}
    end.

get_one_value(Station, Datet, Type) ->
    pollution:get_one_value(Station, Datet, Type, get_monitor()).

get_station_min(Station, Type) ->
    pollution:get_station_min(Station, Type, get_monitor()).

get_daily_mean(Type, Date) ->
    pollution:get_daily_mean(Type, Date, get_monitor()).

get_closest_stations(Station) ->
    pollution:get_closest_stations(Station, get_monitor()).