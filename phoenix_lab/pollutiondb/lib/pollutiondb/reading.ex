require Ecto.Query

defmodule Pollutiondb.Reading do
    use Ecto.Schema

    schema "readings" do
        field :date, :date
        field :time, :time
        field :type, :string
        field :value, :float
        belongs_to :station, Pollutiondb.Station
    end

    def add(station, date, time, type, value) do
        %Pollutiondb.Reading{}
        |> Ecto.Changeset.cast(%{date: date, time: time, type: type, value: value, station_id: station}, [:date, :time, :type, :value, :station_id])
        |> Ecto.Changeset.validate_required([:date, :time, :type, :value, :station_id])
        |> Ecto.Changeset.unique_constraint([:station_id ,:date, :time, :type], name: :readings_station_id_date_time_type_index)
        |> Pollutiondb.Repo.insert
    end

    def add_now(station, type, value) do
        %Pollutiondb.Reading{}
        |> Ecto.Changeset.cast(%{date: Date.utc_today(), time: Time.utc_now(), type: type, value: value, station_id: station}, [:date, :time, :type, :value, :station_id])
        |> Ecto.Changeset.validate_required([:type, :value, :station_id])
        |> Pollutiondb.Repo.insert
    end

    def find_by_date(date) do
        Pollutiondb.Repo.all(
            Ecto.Query.where(Pollutiondb.Reading, date: ^date) )
    end

    def remove(reading) do
        Pollutiondb.Repo.delete(reading)
    end

    def remove_all() do
        Pollutiondb.Repo.delete_all(Pollutiondb.Reading)
    end

    def get_all() do
        Pollutiondb.Repo.all(Pollutiondb.Reading)
    end

    def get_last_ten() do
      Ecto.Query.from(r in Pollutiondb.Reading,
        limit: 10, order_by: [desc: r.date, desc: r.time])
        |> Pollutiondb.Repo.all()
        |> Pollutiondb.Repo.preload(:station)
    end

    def get_last_ten_by_date(date) do
      Ecto.Query.from(r in Pollutiondb.Reading,
        where: r.date == ^date,
        limit: 10,
        order_by: [desc: r.date, desc: r.time])
      |> Pollutiondb.Repo.all()
      |> Pollutiondb.Repo.preload(:station)
    end

    def count_records() do
        Pollutiondb.Repo.aggregate(Pollutiondb.Reading, :count, :id)
    end
end