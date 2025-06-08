defmodule PollutiondbWeb.StationRangeLive do
  use PollutiondbWeb, :live_view

  alias Pollutiondb.Station

  def mount(_params, _session, socket) do
    socket = assign(socket, stations: Station.getAll(), lat_min: -10, lat_max: 10, lon_min: -10, lon_max: 10)
    {:ok, socket}
  end

  def to_float(value, default) do
    case Float.parse(value) do
      {number, ""} -> number
      _ -> default
    end
  end

  def handle_event("update", %{"lat_min" => lat_min, "lat_max" => lat_max, "lon_min" => lon_min, "lon_max" => lon_max}, socket) do
    socket = assign(socket, stations: Station.find_by_location_range(to_float(lon_min, 0.0), to_float(lon_max, 0.0), to_float(lat_min, 0.0), to_float(lat_max, 0.0)), lat_min: lat_min, lat_max: lat_max, lon_min: lon_min, lon_max: lon_max)
    {:noreply, socket}
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

      <form phx-change="update">
      Lon min
      <input type="range" min="-10" max="10" name="lon_min" value={@lon_min}/>
      : <td><%= @lon_min %></td><br/>
      Lon max
      <input type="range" min="-10" max="10" name="lon_max" value={@lon_max}/>
      : <td><%= @lon_max %></td><br/>
      Lat min
      <input type="range" min="-10" max="10" name="lat_min" value={@lat_min}/>
      : <td><%= @lat_min %></td><br/>
      Lat max
      <input type="range" min="-10" max="10" name="lat_max" value={@lat_max}/>
      : <td><%= @lat_max %></td><br/>
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
