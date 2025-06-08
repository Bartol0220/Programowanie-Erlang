defmodule PollutiondbWeb.ReadingLive do
  use PollutiondbWeb, :live_view

  alias Pollutiondb.Reading
  alias Pollutiondb.Station

  def mount(_params, _session, socket) do
    socket = assign(socket, stations: Station.getAll(), readings: Reading.get_last_ten(), date: "", station_id: "", type: "tmp", value: 20.0)
    {:ok, socket}
  end

  def to_float(value, default) do
    case Float.parse(value) do
      {number, ""} -> number
      _ -> default
    end
  end

  def to_int(value, default) do
    case Integer.parse(value) do
      {number, ""} -> number
      _ -> default
    end
  end

  def handle_event("update", %{"date" => date}, socket) do
    readings =
      if date == "" do
        Reading.get_last_ten()
      else
        Reading.get_last_ten_by_date(date)
      end
    socket = assign(socket, readings: readings, date: date)
    {:noreply, socket}
  end

  def handle_event("insert", %{"station_id" => station_id, "type" => type, "value" => value}, socket) do
    Reading.add_now(to_int(station_id, 1), type, to_float(value, 0.0))
    socket = assign(socket, readings: Reading.get_last_ten(), station_id: station_id, type: type, value: value)
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

      Create new reading
      <form phx-submit="insert">
        Station: <select name="station_id">
        <%= for station <- @stations do %>
        <option label={station.name} value={station.id} selected={station.id == @station_id}/>
        <% end %>
        </select><br/>
        Type: <input type="text" name="type" value={@type} /><br/>
        Value: <input type="number" name="value" step="0.1" value={@value} /><br/><br/>
        <input type="submit" />
      </form>

      <br/><br/>

      <form phx-change="update">
      Date: <input type="date" name="date" value={@date} /><br/>
      </form>

      <br/><br/>

      <table>
      <tr>
        <th>Name</th><th>Date</th><th>Time</th><th>Type</th><th>Value</th>
              </tr>
                <%= for reading <- @readings do %>
              <tr>
          <td><%= reading.station.name %></td>
          <td><%= reading.date %></td>
          <td><%= reading.time %></td>
          <td><%= reading.type %></td>
          <td><%= reading.value %></td>
      </tr>
      <% end %>
      </table>
    """
  end
end

