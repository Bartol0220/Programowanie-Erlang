-module(pollution_server_test).
-author("bartol").

-include_lib("eunit/include/eunit.hrl").



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_monitor_test() ->
    pollution_server:start(),
    timer:sleep(10),
    M1 = pollution_server:get_monitor(),
    pollution_server:stop(),
    timer:sleep(10),

    pollution_server:start(),
    timer:sleep(10),
    M2 = pollution_server:get_monitor(),
    ?assertEqual(M1, M2),
    pollution_server:stop(),
    timer:sleep(10).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_station_test() ->
    pollution_server:start(),
    timer:sleep(10),
    ?assertNotMatch({error, _}, pollution_server:add_station("Stacja 1", {1,1})),
    ?assertMatch({error, _}, pollution_server:add_station("Stacja 1", {1,1})),
    ?assertMatch({error, _}, pollution_server:add_station("Stacja 1", {2,2})),
    ?assertMatch({error, _}, pollution_server:add_station("Stacja 2", {1,1})),
    pollution_server:stop(),
    timer:sleep(10).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_value_test() ->
    pollution_server:start(),
    timer:sleep(10),
    pollution_server:add_station("Stacja 1", {1,1}),
    ?assertNotMatch({error, _}, pollution_server:add_value("Stacja 1", calendar:local_time(), "PM10", 46.3)),
    ?assertNotMatch({error, _}, pollution_server:add_value("Stacja 1", calendar:local_time(), "PM1", 46.3)),
    ?assertNotMatch({error, _}, pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3)),
    pollution_server:stop(),
    timer:sleep(10),

    pollution_server:start(),
    timer:sleep(10),
    pollution_server:add_station("Stacja 1", {1,1}),
    pollution_server:add_value("Stacja 1", calendar:local_time(), "PM10", 46.3),
    pollution_server:add_value("Stacja 1", calendar:local_time(), "PM1", 46.3),

    ?assertNotMatch({error, _}, pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3)),

    timer:sleep(1000),
    Time2 = calendar:local_time(),
    ?assertNotMatch({error, _}, pollution_server:add_value( {1,1}, Time2, "PM10", 46.3)),
    ?assertNotMatch({error, _}, pollution_server:add_value( {1,1}, Time2, "PM1", 46.3)),
    ?assertNotMatch({error, _}, pollution_server:add_value( {1,1}, {{2023,3,27},{11,16,10}}, "PM10", 46.3)),
    pollution_server:stop(),
    timer:sleep(10),

    pollution_server:start(),
    timer:sleep(10),
    pollution_server:add_station("Stacja 1", {1,1}),
    pollution_server:add_value({1,1}, Time2, "PM10", 46.3),
    pollution_server:add_value({1,1}, Time2, "PM1", 46.3),
    M2 = pollution_server:add_value({1,1}, {{2023,3,27},{11,16,10}}, "PM10", 46.3),
    ?assertNotMatch({error, _}, M2),
    pollution_server:stop(),
    timer:sleep(10).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_value_fail_test() ->
    pollution_server:start(),
    timer:sleep(10),
    pollution_server:add_station("Stacja 1", {1,1}),
    Time = calendar:local_time(),
    ?assertNotMatch({error, _}, pollution_server:add_value("Stacja 1", Time, "PM10", 46.3)),
    ?assertMatch({error, _}, pollution_server:add_value("Stacja 1", Time, "PM10", 46.3)),
    ?assertMatch({error, _}, pollution_server:add_value("Stacja 1", Time, "PM10", 36.3)),
    ?assertMatch({error, _}, pollution_server:add_value({1,1}, Time, "PM10", 46.3)),
    ?assertMatch({error, _}, pollution_server:add_value({1,1}, Time, "PM10", 36.3)),
    pollution_server:stop(),
    timer:sleep(10).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_value_non_existing_station_test() ->
    pollution_server:start(),
    timer:sleep(10),
    pollution_server:add_station("Stacja 1", {1,1}),
    ?assertMatch({error, _}, pollution_server:add_value("Stacja 2", calendar:local_time(), "PM10", 46.3)),
    ?assertMatch({error, _}, pollution_server:add_value({1,2}, calendar:local_time(), "PM10", 46.3)),
    pollution_server:stop(),
    timer:sleep(10).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_value_test() ->
    pollution_server:start(),
    timer:sleep(10),
    pollution_server:add_station("Stacja 1", {1,1}),
    Time = calendar:local_time(),
    pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),
    pollution_server:add_value("Stacja 1", Time, "PM1", 46.3),
    M3 = pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3),

    M4 = pollution_server:remove_value("Stacja 1", Time, "PM10"),
    ?assertNotMatch({error, _}, M4),
    ?assertNotEqual(M4, M3),
    M5 = pollution_server:remove_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10"),
    ?assertNotMatch({error, _}, M5),
    ?assertNotEqual(M5, M4),
    pollution_server:stop(),
    timer:sleep(10).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_value_and_add_back_test() ->
    pollution_server:start(),
    timer:sleep(10),
    pollution_server:add_station("Stacja 1", {1,1}),
    Time = calendar:local_time(),
    pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),
    pollution_server:add_value("Stacja 1", Time, "PM1", 46.3),
    M3 = pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3),

    M4 = pollution_server:remove_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10"),
    ?assertNotEqual(M4, M3),

    M5 = pollution_server:add_value({1,1}, {{2023,3,27},{11,16,9}}, "PM10", 46.3),
    ?assertEqual(M5, M3),
    pollution_server:stop(),
    timer:sleep(10).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_value_fail_test() ->
    pollution_server:start(),
    timer:sleep(10),
    pollution_server:add_station("Stacja 1", {1,1}),
    Time = calendar:local_time(),
    pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),
    pollution_server:add_value("Stacja 1", Time, "PM1", 46.3),
    pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3),

    ?assertMatch({error, _}, pollution_server:remove_value("Stacja 1", Time, "PM25")),
    ?assertMatch({error, _}, pollution_server:remove_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10")),
    ?assertMatch({error, _}, pollution_server:remove_value({1,2}, Time, "PM10")),
    ?assertMatch({error, _}, pollution_server:remove_value("Stacja 2", Time, "PM10")),
    pollution_server:stop(),
    timer:sleep(10).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_one_value_test() ->
    pollution_server:start(),
    timer:sleep(10),
    pollution_server:add_station("Stacja 1", {1,1}),
    Time = calendar:local_time(),
    pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),
    pollution_server:add_value("Stacja 1", Time, "PM1", 36.3),
    pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 26.3),

    ?assertMatch(46.3, pollution_server:get_one_value("Stacja 1", Time, "PM10")),
    ?assertMatch(36.3, pollution_server:get_one_value("Stacja 1", Time, "PM1")),
    ?assertMatch(46.3, pollution_server:get_one_value({1,1}, Time, "PM10")),
    ?assertMatch(26.3, pollution_server:get_one_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10")),
    pollution_server:stop(),
    timer:sleep(10).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_one_value_fail_test() ->
    pollution_server:start(),
    timer:sleep(10),
    pollution_server:add_station("Stacja 1", {1,1}),
    Time = calendar:local_time(),
    pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),
    pollution_server:add_value("Stacja 1", Time, "PM1", 36.3),
    pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 26.3),

    ?assertMatch({error, _}, pollution_server:get_one_value("Stacja 1", Time, "PM25")),
    ?assertMatch({error, _}, pollution_server:get_one_value({1,1}, Time, "PM25")),
    ?assertMatch({error, _}, pollution_server:get_one_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10")),
    ?assertMatch({error, _}, pollution_server:get_one_value("Stacja 2", Time, "PM1")),
    ?assertMatch({error, _}, pollution_server:get_one_value({1,2}, Time, "PM10")),
    pollution_server:stop(),
    timer:sleep(10).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_station_min_test() ->
    pollution_server:start(),
    timer:sleep(10),
    pollution_server:add_station("Stacja 1", {1,1}),
    pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10),
    pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,11}}, "PM10", 20),

    ?assertMatch(10, pollution_server:get_station_min("Stacja 1", "PM10")),

    pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,12}}, "PM10", 9),

    ?assertMatch(9, pollution_server:get_station_min("Stacja 1", "PM10")),

    pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,13}}, "PM10", 8),

    ?assertMatch(8, pollution_server:get_station_min({1,1}, "PM10")),

    pollution_server:stop(),
    timer:sleep(10).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_station_min_fail_test() ->
    pollution_server:start(),
    timer:sleep(10),
    pollution_server:add_station("Stacja 1", {1,1}),
    ?assertMatch({error, _}, pollution_server:get_station_min("Stacja 1", "PM10")),
    pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10),
    ?assertMatch({error, _}, pollution_server:get_station_min("Stacja 1", "PM25")),
    ?assertMatch({error, _}, pollution_server:get_station_min("Stacja 2", "PM25")),
    pollution_server:stop(),
    timer:sleep(10).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_daily_mean_test() ->
    pollution_server:start(),
    timer:sleep(10),
    pollution_server:add_station("Stacja 1", {1,1}),
    pollution_server:add_station("Stacja 2", {2,2}),
    pollution_server:add_station("Stacja 3", {3,3}),
    pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10),
    pollution_server:add_value("Stacja 2", {{2023,3,27},{11,16,11}}, "PM10", 20),

    ?assertMatch(15.0, pollution_server:get_daily_mean("PM10",{2023,3,27})),

    pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,12}}, "PM10", 10),
    pollution_server:add_value("Stacja 2", {{2023,3,27},{11,16,13}}, "PM10", 20),

    pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,14}}, "PM25", 100),
    pollution_server:add_value("Stacja 2", {{2023,3,27},{11,16,15}}, "PM25", 220),

    ?assertMatch(15.0, pollution_server:get_daily_mean("PM10",{2023,3,27})),

    pollution_server:add_value("Stacja 1", {{2023,3,28},{11,16,16}}, "PM10", 2000),
    pollution_server:add_value("Stacja 2", {{2023,3,28},{11,16,17}}, "PM10", 3000),

    pollution_server:add_value("Stacja 3", {{2023,3,27},{11,16,18}}, "PM10", 1234),

    ?assertMatch(258.8, pollution_server:get_daily_mean("PM10",{2023,3,27})),
    pollution_server:stop(),
    timer:sleep(10).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_daily_mean_fail_test() ->
    pollution_server:start(),
    timer:sleep(10),
    pollution_server:add_station("Stacja 1", {1,1}),
    pollution_server:add_station("Stacja 2", {2,2}),
    ?assertMatch({error, _}, pollution_server:get_daily_mean("PM10",{2023,3,27})),
    pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10),
    pollution_server:add_value("Stacja 2", {{2023,3,27},{11,16,11}}, "PM10", 20),

    ?assertMatch({error, _}, pollution_server:get_daily_mean("PM25",{2023,3,27})),
    ?assertMatch({error, _}, pollution_server:get_daily_mean("PM10",{2023,3,29})),
    pollution_server:stop(),
    timer:sleep(10).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_closest_stations_test() ->
    pollution_server:start(),
    timer:sleep(10),
    pollution_server:add_station("Stacja 1", {1,1}),
    pollution_server:add_station("Stacja 2", {3,3}),


    ?assertMatch(["Stacja 2"], pollution_server:get_closest_stations("Stacja 1")),
    ?assertMatch(["Stacja 1"], pollution_server:get_closest_stations("Stacja 2")),
    ?assertMatch(["Stacja 1"], pollution_server:get_closest_stations({1, 1})),

    pollution_server:add_station("Stacja 3", {2, 2}),

    ?assertMatch(["Stacja 3"], pollution_server:get_closest_stations("Stacja 1")),

    pollution_server:add_station("Stacja 4", {2, 0}),

    ?assertMatch(["Stacja 1"], pollution_server:get_closest_stations({1, 1})),
    ?assertMatch(["Stacja 4", "Stacja 3"], pollution_server:get_closest_stations("Stacja 1")),
    ?assertMatch(["Stacja 2", "Stacja 1"], pollution_server:get_closest_stations("Stacja 3")),
    ?assertMatch(["Stacja 4"], pollution_server:get_closest_stations({2, -2})),
    pollution_server:stop(),
    timer:sleep(10).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_closest_stations_fail_test() ->
    pollution_server:start(),
    timer:sleep(10),

    ?assertMatch({error, _}, pollution_server:get_closest_stations("Stacja 1")),
    ?assertMatch({error, _}, pollution_server:get_closest_stations({1, 1})),

    pollution_server:add_station("Stacja 1", {1,1}),
    pollution_server:add_station("Stacja 2", {3,3}),

    ?assertMatch({error, _}, pollution_server:get_closest_stations("Stacja 3")),

    pollution_server:add_station("Stacja 3", {2, 2}),
    pollution_server:add_station("Stacja 4", {2, 0}),

    ?assertMatch({error, _}, pollution_server:get_closest_stations("Stacja 5")),
    pollution_server:stop().
