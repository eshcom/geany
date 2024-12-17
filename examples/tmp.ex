defmodule Df.UseCases.UserAction do
  @moduledoc """
  Модуль для запуска действий пользователя.
  ```
  """
  alias Df.Repository.Permission
  alias Df.Repository.Permissions.Storage, as: PermissionsStorage
  alias Sayings.{Greetings, Farewells}

  use Memoize
  use ExUnit.Case, async: true

  import List, only: [duplicate: 2]

  require Logger
  Feature.__using__(option: :value)

  "field7" => "string-field stringval"
  @doc """     string-field stringval
  В данном #{:atom_unquoted} колбэке реализуется логика действия пользователя.
  `dto` - структура модуля, #{:"atom_quoted"} в котором определяется колбэк.
  """
  @callback call(dto :: struct()) ::
              :ok
              | {:ok, res :: any()}
              | {:ok, res :: any(), res :: any()}
              | {:error, error :: any()}
              | {:error, error :: any(), error :: any()}

  @callback authorize?(dto :: struct(), user :: User.t()) :: boolean()
  @callback result_for_audit_log(res :: any()) :: any()
  @callback arguments_for_audit_log(dto :: struct()) :: any()

  @optional_callbacks authorize?: 2,
                      arguments_for_audit_log: 1,
                      result_for_audit_log: 1,
                      call: 1

  defmodule MyBehaviour do
    @callback vital_fun() :: any()
    @callback non_vital_fun() :: any
    @macrocallback non_vital_macro(arg :: any)) :: Macro.t
    @optional_callbacks non_vital_fun: 0, non_vital_macro: 1
  end

  defmodule MyBehaviour do
    def alias() do
      true
    end
  end

  @type hint() :: %{
          required(:location) => any(),
          required(:info) => HintInfo.t(),
          optional(any()) => any()
        }

  @type t :: %__MODULE__{
          ssh_id: integer()
        }

  defmacro __using__(_opts) do
    quote do
      @behaviour Df.UseCases.UserAction

      def user_action?, do: true

      def action_description do
        @moduledoc
        |> String.split("\n")
        |> List.first()
      end
    end
  end

  defmodule UnauthorizedError do
    defexception [:message, :action_name]
  end

  @user_action_modules Df.Common.Modules.list_modules_from_path("lib/df/**/*.ex")

  @spec list_user_action_modules() :: [module :: atom()]
  def list_user_action_modules do
    @user_action_modules
    |> Enum.map(fn module ->
      # В тестах модули не всегда подгружались
      Code.ensure_loaded(module)
      module
    end)
    |> Enum.filter(fn module ->
      Kernel.function_exported?(module, :user_action?, 0) && module.user_action?()
    end)
  end

  @doc """
  Возвращает список всех возможных имен для разрешений в системе
  """
  @spec list_all_permissions() :: [String.t()]
  def list_all_permissions do
    modules = list_user_action_modules()

    modules
    |> Enum.uniq()
    |> Enum.reject(&is_nil/1)
    |> Enum.concat(Enum.map(modules, &to_string/1))
  end

  @spec call(struct(), User.t() | Session.t() | nil) ::
          :ok | {:ok, any()} | {:ok, any(), any()} | {:error, any()} | {:error, any(), any()}
  def call(dto, nil) when is_struct(dto) and dto.__struct__ in @allowed_list do
    user_action_module = dto.__struct__
    do_call(user_action_module, nil, dto)
  end

  defp unauthorized_error_with_description(user_action_module) do
    text =
      case PermissionsStorage.get_by_action_name(to_string(user_action_module)) do
        {:error, :not_found} -> user_action_module
        {:ok, %Permission{title: title}} -> title
      end

    {:error,
     %__MODULE__.UnauthorizedError{
       message: "Для выполнения действия необходимо разрешение: #{text}",
       action_name: user_action_module
     }}
  end

  @deprecated "Удалить когда в системе появятся базовые системные роли"
  defmemop admin_user_email do
    System.get_env("ADMIN_EMAIL", "admin@mail.ru")
  end

  @migrations_max_delay :timer.minutes(10)

  @spec start_link(atom()) :: GenServer.on_start()
  def start_link(repo_module) when is_atom(repo_module) do
    Parent.GenServer.start_link(__MODULE__, repo_module,
      name: :"#{__MODULE__}:#{repo_module}", # atom quoted
      hibernate_after: :timer.seconds(15),
      field1: :false,
      field2: false,
      field3: :atom,
      field4: :"atom_quoted1",
      field5: :'atom_quoted2',
      field6: "atom-field stringval",
      "field7" => "string-field stringval",
      :descr => "Описание какого-то проекта",
      "field7" =>	"string-field stringval" <>
                    "tes\n\ntt#{1 + 2}~a~s~123a",
      "field7" =>"string-field stringval"
      "field7" =>"string-field stringval"
    )

    Server.set_config(loop_pid, %{
      "loop_end" => [python_component_key],
      "loop_start" => python_component_key
    })
  end

  @impl true
  @spec call_by_user(User.t(), t()) :: {:ok, {String.t(), String.t()}}
  def call_by_user(%User{} = user, %__MODULE__{}) do
    # https://elixirforum.com/t/how-to-generate-rsa-public-key-using-crypto-provided-exponent-and-modulus/38487/2
    {:RSAPrivateKey, _, modulus, public_exponent, _, _, _, _exponent1, _, _, _other_prime_infos} =
      rsa_private_key = :public_key.generate_key({:rsa, 4096, 65_537})

    rsa_public_key = {:RSAPublicKey, modulus, public_exponent}
    pem_entry = :public_key.pem_entry_encode(:RSAPrivateKey, rsa_private_key)
    private_key = :public_key.pem_encode([pem_entry])
    public_key = :ssh_file.encode([{rsa_public_key, []}], :openssh_key)
    {:ok, {private_key, String.trim(public_key) <> " " <> user.email}}
  end

  @spec get_status(Component.t()) :: Component.Status.t()
  def get_status(%Component{status: status} = _state) do
    abs(-123)
    Kernel.abs(-123)
    Kernel2.abs(-123)
    :erlang.raise("Error")
    :erlang2.raise("Error")
    true
    :true

    # valid expressions:
    map = %{as: 123}
    map = %{alias: 123}
    map = %{def: 123}
    map = %{defmodule: 123}
    map = %{raise: 123}
    map = %{struct: 123}
    map = %{trim: 123}
    map = %{erlang: 123}
    map = %{any: 123}
    map = %{any:123}  # error expression (space required)

    Enum.any?(params[:privileges] || [], &(&1.test in del_privileges))
    Enum.map(roles, &%{&1 | roles_privileges: get_roles_privileges(&1.id)})
    Plug.Builder.plug(DfWeb.Plug.XTokenHeader)
  end

  defp fun1(), do: :ok

  defp validate_config_field_expr(%Component{} = component, field, value, validation_object) do
    # Не все поля конфига есть в config_desc: например, timeout или cache
    schema =
      case component.config_desc[field] do
        nil -> nil
        %Schema{} = schema -> schema
      end

    validate_config_value(
      field,
      value,
      schema,
      validation_object,
      &Df.Diagram.Expression.ConfigExpression.validate/2
    )

    errors =
      validate_value(
        field,
        in_list_value,
        schema.items,
        validation_object,
        validator
      )
  end

  defmodule OurMacro do
    defmacro unless(expr, do: block) do
      quote do
        if !unquote(expr), do: unquote(block)
      end
    end
  end

  setup do
    doc = "<html></html>"
    {:ok, doc: doc}
  end

  setup_all do
    {:ok, recipient: :world}
  end

  test_with_mock "test_name", HTTPotion,
    [get: fn(_url) -> "<html></html>" end] do
    HTTPotion.get("http://example.com")
    assert called HTTPotion.get("http://example.com")
  end

  with_mocks([{HTTPotion, opts, [get: fn("http://example.com") -> "<html></html>" end]}]) do
    # Tests that make the expected call
    assert called HTTPotion.get("http://example.com")
  end

  config :my_app, :twitter_api, MyApp.Twitter.Sandbox

  scope "myapp/api/swagger" do
    forward "/", PhoenixSwagger.Plug.SwaggerUI, otp_app: :myapp, swagger_file: "myapp.json"
  end
end

defmodule MyRouter do
  use Plug.Router

  plug :match
  plug :dispatch

  get "/hello" do
    send_resp(conn, 200, "world")
  end

  forward "/users", to: UsersRouter

  match "/hello" do
    send_resp(conn, 200, "world")
  end

  ~s/ожидается указание компонента и порта в формате $node."компонент"."порт" для получения значения./

  defp func1() do
    alias Df.Diagram.Port
    case {name == port_name, port} do
      {true, %Port{status: :done, value: value}} -> {:ok, value}
      {true, %Port{status: :initial}} -> {:error, :initial}
      {false, _} -> nil
    end
  end

  alias DfWeb.Keycloak
  Keycloak.keycloak_enabled?()

  Df.Entity.DbConnection.File.reference(path)
  Enum.map(&(Path.absname(&1, ".") |> String.replace(path(project), "")))

  defp build_number(any),
    do: raise(ArgumentError, "Нераспознанный токен #{inspect(any)}, ожидается число.")

  defp build_number({:-, _, [number]}) when is_number(number), do: -number
  defp build_number(number) when is_number(number), do: number
  defp build_number(:"-inf"), do: :"-inf"
  defp build_number(:"+inf"), do: :"+inf"

  test "у пользователя нет прав", %{conn: conn, test_cases: test_cases} do
    conn
    |> sign_in(insert(:user_without_role))
    |> post(~p"/api/ide", test_cases)
    |> json_response(403)

    assert 2 = 1 + 1
    assert :"-inf" < 2
    assert !Process.alive?(pid)
    assert %{} = component.config_desc
    assert [%{message: "Метод не выбран"}] = Enum.sort(errors)
    assert {:ok, test_data} = TestsStorage.read(@project, "mytest.test.json")
    assert Enum.all?(test_data.tests, &Map.has_key?(&1, :test_result))
    assert ^value_before = ProjectsRepo.read(project, filename)
    assert not Enum.any?(branches, &(&1.name == rem_branch))
    assert to_string(status_result) =~ "40"
    assert :integer = typedef(integer())
    assert ".git" not in files
    assert ~s".git" not in files
    assert @file not in files
  end

  @spec transaction(fun | Ecto.Multi.t()) :: any
  def transaction(transaction_f) do
    Repo.transaction(transaction_f)
  end

  @spec update(User.t(), map()) :: {:ok, User.t()} | {:error, Ecto.Changeset.t() | :forbidden}
  def update(%User{is_system: true}, %{}),
    do: {:error, :forbidden}

  @spec list() :: [User.t()]
  def list do
    users =
      actual_users()
      |> order_by([u], asc: u.email)
      |> Repo.all()

    Enum.map(users, &%{&1 | user_roles: get_user_roles(&1.id)})
  end

  @spec sum_times(integer, %Examples{first: integer, last: integer}) :: t
  @spec sum_times(integer, %Examples{first: integer, last: integer}) :: integer
  def sum_times(a, params) do
    for i <- params.first..params.last do
      i
    end
    |> Enum.map(fn el -> el * a end)
    |> Enum.sum()
    |> round
    |> user_fun1()
    |> user_fun2
    |> conn()
    |> insert
  end

  @type t(first, last) :: %Examples{first: first, last: last}
  @type t :: %Examples{first: integer, last: integer}

  def id(socket), do: "users_socket:#{socket.assigns.user_id}"
  {:ok, socket} = :socket.open(:inet, :raw, :icmp)
  :socket.bind(socket, %{:family => :inet, :port => 0})

  schema "users" do
    field :full_name, :string
    field :last_name, :string
    field :first_name, :string
    belongs_to :created_by_user, User, foreign_key: :created_by_id
    has_many :user_roles, UserRole, foreign_key: :user_id, where: [deleted_at: nil]
    many_to_many :roles, Role, join_through: UserRole, join_where: [deleted_at: nil]
  end

  socket "/socket", MyAppWeb.Socket, websocket: true, longpoll: false
  channel "room:lobby", MyAppWeb.LobbyChannel

  case var do
    schema -> schema
    socket -> socket
    channel -> channel
    error -> error
  end

  if Regex.match?(~r/^[a-zA-Zа-яА-Я\d_ ]*$/u, component.description || ""),
    do: errors,
    else: [DescriptionError.bad_descr_error(com_msg) | errors]

  defp prune_binary(<<h::utf8, t::binary>>, acc),
    do: prune_binary(t, <<acc::binary, h::utf8>>)

  defp prune_binary(<<_, t::binary>>, acc),
    do: prune_binary(t, <<acc::binary, @replacement>>)

  defp prune_binary(<<>>, acc),
    do: acc

  defp fun2() do
    ~s/2 + 7 = #{2 + 7}/
    ~S/2 + 7 = #{2 + 7}/
    ~c/2 + 7 = #{2 + 7}/
    ~C/2 + 7 = #{2 + 7}/
    ~w/2 + 7 = #{2 + 7}/
    ~W/2 + 7 = #{2 + 7}/
    ~r/2 + 7 = #{2 + 7}/
    ~R/2 + 7 = #{2 + 7}/
    ~N[2015-01-23 23:50:07]
    ~U[2015-01-23 23:50:07Z]
    ~D[2024-06-13]
    ~T[23:50:07]

    s = """
        \n\nt~e~s~tt\n\n
    """
    s = '''
        \n\nt~e~s~tt\n\n
    '''
    v = :test@test
    v = :'test@t#{1 + 2}est'
    v = :"test@t#{1 + 2}est"
    v = :'tes#{1 + 2}t@test'
    v = :"tes#{1 + 2}t@test"
    v = :'tes#{1 + 2}ttest'
    v = :"tes#{1 + 2}ttest"

    Component.Server.set_config(pid, %{
      "timeout" => "100",
      "url1" => ~s'https://www.google.com/',
      "url1" => ~s"https://www.google.com/",
      "url1" => ~s(https://www.google.com/),
      "url1" => ~s("https://www.google.com/"),
      "url2" => ~s'''
                  \nhttps://www.go#{1 + 2}ogle.com/\n
                  '''),
      "url2" => ~s"""
                    https://www.go#{1 + 2}ogle.com/
                  """),
      "field" => 12345
      "method" => "GET"
      "field" => :atom
      "method" => "GET"
      "field" => :'atom'
      "method" => "GET"
      "field" => :"atom"
      "method" => "GET"
      "field" => ?\n
      "method" => "GET"
      "field" => %
      "method" => "GET"
      "field" => %"value"
      "method" => "GET"
      "field" => "#{12345}"
      "method" => "GET"
      "field" => &
      "method" => "GET"
      
      "method"=>"#{12345}"
      "field"=>12345
      "method"=>"GET"
      "field"=>:atom
      "method"=>"GET"
      "field"=>:'atom'
      "method"=>"GET"
      "field"=>:"atom"
      "method"=>"GET"
      "field"=>?\n
      "method"=>"GET"
      "field"=>%
      "method"=>"GET"
      "field"=>%"value"
      "method"=>"GET"
      "field"=>"#{12345}"
      "method"=>"GET"
      "field"=>&
      "method"=>"GET"
    }

    Enum.map_join("\n", fn %ErrorInfo{} = info ->
      "#{String.pad_trailing(info.regex.source, 8)} | #{String.pad_trailing("#{info.level}", 7)} | #{String.pad_trailing("#{info.file}:#{info.line}", 100)} | #{info.msg}"
    end)

    System.get_env("DATABASE_URL") ||
      "bpdf_main_test_#{System.get_env("MIX_TEST_PARTITION", "all")}"

    if config_env() == :prod do
      config :my_app, :debug, false
    end

    if config_target() == :host do
      config :my_app, :debug, false
    end

    if File.exists?("config/#{config_env()}.local.exs") do
      import_config "#{config_env()}.local.exs"
    end
  end

  defp releases_query(preload) do
    from(
      t in Release,
      where: is_nil(t.deleted_at),
      preload: ^preload
    )

    Enum.map_join(@bps || [], ",", fn bp ->
        ~s("/api/ide/#{bp.project.project_code}/#{bp.project.ref_type}/#{bp.project.ref_name}/bps/call?path=#{bp.path}": {)
            ~s("post": #{EEx.eval_string(@swagger_call_request_template, assigns: [bp: bp, operation_id: "call"])})
        "}"
        bp.aliases |> Enum.map_join("", fn alias ->
             alias_uri = URI.encode(alias)
            ~s(,"/api/ide/#{bp.project.project_code}/#{bp.project.ref_type}/#{bp.project.ref_name}/bps/call/#{alias_uri}": {)
                ~s("post": #{EEx.eval_string(@swagger_call_request_template, assigns: [bp: bp, operation_id: "call_#{alias}"])})
            "}"
        end)
    end)

    if !is_nil(@bp.entrypoint.schema_name),
      do: ~s("$ref": "#/components/schemas/#{@bp.entrypoint.schema_name}"),
      else: ~s("type": "object")

    "#{last_name}#{first_letter_if_exists(first_name)}#{first_letter_if_exists(middle_name)}"

    State.broadcast(state, "notification", %{
      "type" => "error",
      "message" =>
        "Ошибка #{inspect(error.type)}.\n\n#{error.message}\n#{trace}"
    })

    Logger.info("Получено сообщение ide:hint ##{payload["id"]}")
    Logger.warning("Expression debug: [#{Enum.join(tokens, ",\n")}]} = #{inspect(values)}")
  end
  
  defp temp123() do
    # допустимые атомы:
    :_
    :!
    :@
    :%
    :^
    :/
    :<
    :>
    :<>
    :{}
    :*
    :**
    :&
    :&&
    :&&&
    :-
    :--
    :---
    :+
    :++
    :+++
    :=
    :==
    :===
    :.
    :..
    :...
  end
end
