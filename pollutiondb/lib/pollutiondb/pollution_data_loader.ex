defmodule Pollutiondb.PollutionDataLoader do
    def parsline(line) do
        [datetime_str, type, val_str, id, name, cords] = String.split(line, ";")

        [date, time] = datetime_str |> String.split("T")
        time = time |> String.slice(0, 8)

        date = date
               |> String.split("-")
               |> Enum.map(&String.to_integer/1)
               |> List.to_tuple()
               |> Date.from_erl!()

        time = time
               |> String.split(":")
               |> Enum.map(&String.to_integer/1)
               |> List.to_tuple()
               |> Time.from_erl!()

        val = val_str |> String.to_float()

        {lon, lat} = cords
                |> String.split(",")
                |> Enum.map(&String.to_float/1)
                |> List.to_tuple()

        %{id: String.to_integer(id), value: val, type: type, date: date, time: time, name: name, lon: lon, lat: lat}
    end

    def load_data_from_file(path) do
        File.read!(path)
        |> String.trim()
        |> String.split("\n")
        |> Enum.map(&Pollutiondb.PollutionDataLoader.parsline/1)
    end

    def add_all_stations(map) do
        map
        |> Enum.uniq_by(& &1.id)
        |> Enum.map(fn x -> Pollutiondb.Station.add(x.name <> " " <> Integer.to_string(x.id), x.lon, x.lat) end)
    end

    def add_all_values(map) do
        map
        |> Enum.map(fn x ->
            station = Pollutiondb.Station.find_by_name(
                          x.name <> " " <> Integer.to_string(x.id)
                      )
                      |> List.first()

            Pollutiondb.Reading.add(
                station.id,
                x.date,
                x.time,
                x.type,
                x.value
            )
        end)
    end

    def load_all_data(path) do
        map = Pollutiondb.PollutionDataLoader.load_data_from_file(path)

        Pollutiondb.PollutionDataLoader.add_all_stations(map)
        Pollutiondb.PollutionDataLoader.add_all_values(map)
        :ok
    end



    def load_data_pc() do
        Pollutiondb.PollutionDataLoader.load_all_data("D:\\nauka\\Programowanie-Erlang\\data\\AirlyData-ALL-50k.csv")
    end

    def load_data_laptop() do
        Pollutiondb.PollutionDataLoader.load_all_data("C:\\Users\\Admin\\studia\\Programowanie-Erlang\\data\\AirlyData-ALL-50k.csv")
    end
end
