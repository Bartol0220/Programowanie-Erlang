-module(pollution_gen_server).
-author("Bartol").
-behavior(gen_server).

%% API
-export([start_link/0, init/1, handle_call/3, handle_cast/2, crash/0, get_monitor/0, add_station/2, add_value/4,
    remove_value/3, get_one_value/3, get_station_min/2, get_daily_mean/2, get_closest_stations/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    M = pollution:create_monitor(),
    {ok, M}.


handle_call({getMonitor}, _From, Value) ->
    {reply, Value, Value};
handle_call({get_one_value, Station, Datet, Type}, _From, Value) ->
    Res = pollution:get_one_value(Station, Datet, Type, Value),
    {reply, Res, Value};
handle_call({get_station_min, Station, Type}, _From, Value) ->
    Res = pollution:get_station_min(Station, Type, Value),
    {reply, Res, Value};
handle_call({get_daily_mean, Type, Date}, _From, Value) ->
    Res = pollution:get_daily_mean(Type, Date, Value),
    {reply, Res, Value};
handle_call({get_closest_stations, Station}, _From, Value) ->
    Res = pollution:get_closest_stations(Station, Value),
    {reply, Res, Value}.


handle_cast({addStation, Name, Coordinates}, State) ->
    NM = pollution:add_station(Name, Coordinates, State),
    case NM of
        {error, _} -> {noreply, State};
        _ -> {noreply, NM}
    end;
handle_cast({add_value, Station, Datet, Type, Value}, State) ->
    NM = pollution:add_value(Station, Datet, Type, Value, State),
    case NM of
        {error, _} -> {noreply, State};
        _ -> {noreply, NM}
    end;
handle_cast({remove_value, Station, Datet, Type}, State) ->
    NM = pollution:remove_value(Station, Datet, Type, State),
    case NM of
        {error, _} -> {noreply, State};
        _ -> {noreply, NM}
    end;
handle_cast({crash}, State) ->
    ala:makota(),
    {noreply, State}.



crash() ->
    gen_server:cast(pollution_gen_server, {crash}).



get_monitor() ->
    gen_server:call(pollution_gen_server, {getMonitor}).



add_station(Name, Coordinates) ->
    gen_server:cast(pollution_gen_server, {addStation, Name, Coordinates}).

add_value(Station, Datet, Type, Value) ->
    gen_server:cast(pollution_gen_server, {add_value, Station, Datet, Type, Value}).

remove_value(Station, Datet, Type) ->
    gen_server:cast(pollution_gen_server, {remove_value, Station, Datet, Type}).



get_one_value(Station, Datet, Type) ->
    gen_server:call(pollution_gen_server, {get_one_value, Station, Datet, Type}).

get_station_min(Station, Type) ->
    gen_server:call(pollution_gen_server, {get_station_min, Station, Type}).

get_daily_mean(Type, Date) ->
    gen_server:call(pollution_gen_server, {get_daily_mean, Type, Date}).

get_closest_stations(Station) ->
    gen_server:call(pollution_gen_server, {get_closest_stations, Station}).