defmodule PollutiondbWeb.StationLive do
  use PollutiondbWeb, :live_view

  alias Pollutiondb.Station

  def mount(_params, _session, socket) do
    socket = assign(socket, stations: Station.getAll(), name: "", lat: "", lon: "", name_find: "")
    {:ok, socket}
  end

  def to_float(value, default) do
    case Float.parse(value) do
      {number, ""} -> number
      _ -> default
    end
  end

  def handle_event("insert", %{"name" => name, "lat" => lat, "lon" => lon}, socket) do
    Station.add(%Station{name: name, lat: to_float(lat, 0.0), lon: to_float(lon, 0.0)})
    socket = assign(socket, stations: Station.getAll(), name: name, lat: lat, lon: lon)
    {:noreply, socket}
  end

  def handle_event("search", %{"name_find" => name}, socket) do
    stations =
      if name == "" do
        Station.getAll()
      else
        Station.find_by_name_prefix(name) # Inna wersja Station.find_by_name(name), bo szuka wszystkich stacji,
                                          # które zaczynają się od podanego prefixu (ulepszona wersja zadania z labów)
      end
    {:noreply, assign(socket, stations: stations, name_find: name)}
  end

  def render(assigns) do
    ~H"""

      <.link href="/">
      <button style="border: 2px solid black; padding: 4px 4px;">View stations</button>
      </.link>
      <.link href="/range">
      <button style="border: 2px solid black; padding: 4px 4px;">Stations by position</button>
      </.link>
      <.link href="/readings">
      <button style="border: 2px solid black; padding: 4px 4px;">Stations readings</button>
      </.link>
      <br/><br/>

      Create new station
      <form phx-submit="insert">
        Name: <input type="text" name="name" value={@name} /><br/>
        Lat: <input type="number" name="lat" step="0.1" value={@lat} /><br/>
        Lon: <input type="number" name="lon" step="0.1" value={@lon} /><br/><br/>
        <input type="submit" />
      </form>

      <br/><br/>

      Find Station by name
      <form  phx-change="search">
        Name: <input type="text" name="name_find" value={@name_find} /><br/><br/>
      </form>

      <br/><br/>

      <table>
      <tr>
        <th>Name</th><th>Longitude</th><th>Latitude</th>
              </tr>
              <%= for station <- @stations do %>
                <tr>
          <td><%= station.name %></td>
          <td><%= station.lon %></td>
          <td><%= station.lat %></td>
        </tr>
      <% end %>
    </table>
    """
end
end