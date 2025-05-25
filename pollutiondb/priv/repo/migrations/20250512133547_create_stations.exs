defmodule Pollutiondb.Repo.Migrations.CreateStations do
  use Ecto.Migration

  def change do
    create table(:stations) do
      add :name, :string
      add :lon, :float
      add :lat, :float
    end

    create unique_index(:stations, [:name])
    create unique_index(:stations, [:lon, :lat])
  end
end
