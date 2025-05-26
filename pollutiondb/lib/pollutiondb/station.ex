require Ecto.Query
defmodule Pollutiondb.Station do
    use Ecto.Schema

    schema "stations" do
        field :name, :string
        field :lon, :float
        field :lat, :float
        has_many :readings, Pollutiondb.Reading
    end

    def add(station) do
        Pollutiondb.Repo.insert(station)
    end

    def add_stations(stations) do
        stations |>
        Enum.map(fn x -> Pollutiondb.Station.add(x) end)
    end

    def get_all() do
        Pollutiondb.Repo.all(Pollutiondb.Station)
    end

    def count_records() do
        Pollutiondb.Repo.aggregate(Pollutiondb.Station, :count, :id)
    end

    def get_by_id(id) do
        Pollutiondb.Repo.get(Pollutiondb.Station, id)
        |> Pollutiondb.Repo.preload(:readings)
    end

    def remove(station) do
        Pollutiondb.Repo.delete(station)
    end

    def remove_all() do
        Pollutiondb.Repo.delete_all(Pollutiondb.Station)
    end

    def find_by_name(name) do
        Pollutiondb.Repo.all(
            Ecto.Query.where(Pollutiondb.Station, name: ^name) )
    end

    def find_by_location(lon, lat) do
        Ecto.Query.from(s in Pollutiondb.Station,
            where: s.lon == ^lon,
            where: s.lat == ^lat)
        |> Pollutiondb.Repo.all
    end

    def find_by_location_range(lon_min, lon_max, lat_min, lat_max) do
        Ecto.Query.from(s in Pollutiondb.Station,
            where: s.lon >= ^lon_min,
            where: s.lon <= ^lon_max,
            where: s.lat >= ^lat_min,
            where: s.lat <= ^lat_max)
        |> Pollutiondb.Repo.all
    end

    defp changeset(station, changesmap) do
        station
        |> Ecto.Changeset.cast(changesmap, [:name, :lon, :lat])
        |> Ecto.Changeset.validate_required([:name, :lon, :lat])
        |> Ecto.Changeset.validate_number(:lon, greater_than_or_equal_to: -180, less_than_or_equal_to: 180)
        |> Ecto.Changeset.validate_number(:lat, greater_than_or_equal_to: -90, less_than_or_equal_to: 90)
        |> Ecto.Changeset.unique_constraint(:name)
        |> Ecto.Changeset.unique_constraint([:lon, :lat], name: :stations_lon_lat_index)
    end

#    def update_name(station, newname) do
#        Ecto.Changeset.cast(station, %{name: newname}, [:name])
#        |> Ecto.Changeset.validate_required([:name])
#        |> Pollutiondb.Repo.update
#    end

    def update_name(station, newname) do
        changeset(station, %{name: newname})
        |> Pollutiondb.Repo.update
    end

#    def add(name, lon, lat) do
#        %Pollutiondb.Station{}
#        |> Ecto.Changeset.cast(%{name: name, lon: lon, lat: lat}, [:name, :lon, :lat])
#        |> Ecto.Changeset.validate_required([:name, :lon, :lat])
#        |> Ecto.Changeset.validate_number(:lon, greater_than_or_equal_to: -180, less_than_or_equal_to: 180)
#        |> Ecto.Changeset.validate_number(:lat, greater_than_or_equal_to: -90, less_than_or_equal_to: 90)
#        |> Pollutiondb.Repo.insert
#    end

    def add(name, lon, lat) do
        %Pollutiondb.Station{}
        |> changeset(%{name: name, lon: lon, lat: lat})
        |> Pollutiondb.Repo.insert
    end
end
