-module(kalkulatorZanieczyszczen).
-author("bartol").

%% API
-export([test_data/0, number_of_readings/2, calculate_max/2, calculate_mean/2]).

test_data() ->
  [
    {
      "stacja1", {2024, 3, 10}, {11, 0, 0}, [{pm10, 10}, {pm2_5, 10}, {pm1, 7}, {temp, 2}, {press, 1017}, {hum, 79}]
    },
    {
      "stacja2", {2024, 3, 10}, {12, 30, 10}, [{pm10, 25}, {pm2_5, 32}, {pm1, 12}, {temp, 8}, {press, 1013}]
    },
    {
      "stacja1", {2024, 3, 11}, {11, 0, 0}, [{pm10, 8}, {pm2_5, 5}, {pm1, 2}, {temp, 10}, {press, 1020}, {hum, 69}]
    },
    {
      "stacja1", {2024, 3, 12}, {11, 0, 0}, [{pm10, 15}, {pm2_5, 17}, {pm1, 9}, {temp, 12}, {press, 1022}, {hum, 71}]
    },
    {
      "stacja2", {2024, 3, 15}, {18, 42, 42}, [{pm10, 9}, {pm2_5, 9}, {pm1, 13}, {temp, 12}, {press, 1016}]
    },
    {
      "stacja3", {2024, 3, 8}, {7, 17, 37}, [{pm10, 6}, {pm2_5, 7}]
    },
    {
      "stacja3", {2024, 3, 8}, {17, 18, 9}, [{pm10, 7}, {pm2_5, 8}]
    },
    {
      "stacja5", {2024, 3, 15}, {20, 6, 21}, [{pm10, 2}, {pm2_5, 3}, {temp, 19}]
    },
    {
      "stacja2", {2024, 3, 19}, {9, 13, 34}, [{pm10, 5}, {pm2_5, 4}, {pm1, 2}, {temp, 16}, {press, 1012}]
    },
    {
      "stacja3", {2024, 3, 10}, {8, 27, 25}, [{pm10, 10}, {pm2_5, 10}]
    },
    {
      "stacja4", {2024, 3, 19}, {14, 0, 0}, [{pm10, 81}, {pm2_5, 50}, {pm1, 26}, {press, 1017}]
    },
    {
      "stacja4", {2024, 3, 11}, {15, 0, 0}, [{pm10, 42}, {pm2_5, 38}, {pm1, 16}, {press, 1019}]
    },
    {
      "stacja4", {2024, 3, 12}, {10, 0, 0}, [{pm10, 52}, {pm2_5, 40}, {pm1, 21}, {press, 1015}]
    }
  ].



number_of_readings(Readings, Date) -> number_of_readings(Readings, Date, 0).
number_of_readings([], _, Sum) -> Sum;
number_of_readings([H | T], Date, Sum) ->
  CurrDate = element(2, H),
  case CurrDate of
    Date -> number_of_readings(T, Date, Sum + 1);
    _ -> number_of_readings(T, Date, Sum)
  end.



greater_list_type([], _, Max) -> Max;
greater_list_type([H | T], Type, Max) ->
  case element(1, H) of
    Type -> max(Max, element(2, H));
    _ -> greater_list_type(T, Type, Max)
  end.

calculate_max(Readings, Type) -> calculate_max(Readings, Type, 0).
calculate_max([], _, Max) -> Max;
calculate_max([H | T], Type, Max) ->
  Measur = element(4, H),
  calculate_max(T, Type, greater_list_type(Measur, Type, Max)).



find_type([], _) -> null;
find_type([H | T], Type) ->
  case element(1, H) of
    Type -> element(2, H);
    _ -> find_type(T, Type)
  end.

sum_and_count_type([], _, Sum, Cnt) -> {Sum, Cnt};
sum_and_count_type([H | T], Type, Sum, Cnt) ->
  Measur = element(4, H),
  Value = find_type(Measur, Type),
  case Value of
    null -> sum_and_count_type(T, Type, Sum, Cnt);
    _ -> sum_and_count_type(T, Type, Sum + Value, Cnt + 1)
  end.

calculate_mean(Readings, Type) ->
  {Sum, Cnt} = sum_and_count_type(Readings, Type, 0, 0),
  case Cnt of
    0 -> 0;
    _ -> Sum/Cnt
  end.
