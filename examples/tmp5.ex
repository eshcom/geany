defmodule DfAsync.RabbitMqConnection do
  @moduledoc """
  GenServer для подключения к RMQ
  """
  use GenServer

  require Logger

  defmodule State do
    @moduledoc false
    defstruct [:connection]

    @type t() :: %__MODULE__{connection: %AMQP.Connection{} | nil}
  end

  def get_connection(), do: GenServer.call(__MODULE__, :get_connection)

  def start_link(_opts), do: GenServer.start_link(__MODULE__, nil, name: __MODULE__)

  @impl GenServer
  def init(_opts), do: {:ok, %State{}}
end
