-module(pollution_value_collector_gen_statem).
-author("bartol").
-behavior(gen_statem).

%% API
-export([init/1, start_link/0, callback_mode/0, set_station/2, add_value/2, store_data/0, waiting_station/3,
    waiting_first_value/3, waiting_store_data/3]).

-record(data, {station, values = []}).



init(_) ->
    {ok, waiting_station, #data{station = "", values = []}}.

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

callback_mode() ->
    state_functions.



set_station(Name, {C1, C2}) -> gen_statem:cast(pollution_value_collector_gen_statem, {station, Name, {C1, C2}}).
add_value(Type, Value) -> gen_statem:cast(pollution_value_collector_gen_statem, {value, Type, Value}).
store_data() -> gen_statem:cast(pollution_value_collector_gen_statem, data).



waiting_station(_, {station, Name, Coordinates}, State) ->
    NewState = State#data{station = {Name, Coordinates}},
    io:format("New station ~w~n", [Name]),
    {next_state, waiting_first_value, NewState}.

waiting_first_value(_, {value, Type, Value}, State) ->
    NewState = State#data{values = [{Type, Value, calendar:local_time()}]},
    io:format("New value: type ~w, value: ~w~n", [Type, Value]),
    {next_state, waiting_store_data, NewState}.

waiting_store_data(_, {value, Type, Value}, State) ->
    NewState = State#data{values = [{Type, Value, calendar:local_time()} | State#data.values]},
    io:format("New value: type ~w, value: ~w~n", [Type, Value]),
    {next_state, waiting_store_data, NewState};
waiting_store_data(_, data, State) ->
    {Name, Coordinates} = State#data.station,
    Values = State#data.values,
    pollution_gen_server:add_station(Name, Coordinates),
    [pollution_gen_server:add_value(Name, Datet, Type, Value) || {Type, Value, Datet} <- Values],
    NewState = #data{station = "", values = []},
    {next_state, waiting_station, NewState}.
