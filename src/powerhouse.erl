%%%-------------------------------------------------------------------
%% @doc Powerhouse
%%%-------------------------------------------------------------------

-module(powerhouse).
-behaviour(gen_server).

%% Application callbacks
-compile([export_all]).

%%====================================================================
%% Powerhouse - API
%%====================================================================

% @doc Włączanie wszystkich wiatraków podpiętych do elektrowni
windmills_on() ->
    gen_server:cast(?MODULE, start).

% @doc Blokowanie wszystkich wiatraków podpiętych do elektrowni
windmills_off() ->
    gen_server:cast(?MODULE, stop).

% @doc Tworzenie N nowych klientów
new_clients(N) ->
    [client:create() || _ <- lists:seq(1,N)].    

% @doc Wyświetlanie aktualnego stanu elektrowni w formacie {Zgromadzona energia, ilość wiatraków}
state() ->
    gen_server:call(?MODULE, user_friendly_state).

%%====================================================================
%% Powerhouse - internal
%%====================================================================

start_windmills(L) ->
    lists:foreach(fun(X) -> windmill:start_rotating(X) end, L).

stop_windmills(L) ->
    lists:foreach(fun(X) -> windmill:stop_rotating(X) end, L).

% @doc Funkcja tworząca collector w osobnym wątku
collect_energy() ->
    spawn(powerhouse, collector, []). 

% @doc Odpowiada za zbieranie energii z wiatraków i wysyłanie sumy tej energii do gen_servera w celu aktualizacji jego stanu
collector() ->
    {Energy, L} = gen_server:call(?MODULE, state),
    CollectedEnergy = [X || X <- lists:map(fun(Y) -> windmill:get_energy(Y) end, L)],
    NewEnergy = lists:foldl(fun(X, Sum) -> X + Sum end, 0, CollectedEnergy),
    ?MODULE ! {producer, Energy + NewEnergy},
    timer:sleep(2000), 
    collector().

% @doc Konsumuje energię ze stanu
consume_energy(Amount) ->
    gen_server:call(?MODULE, {send_energy, Amount}).

%%====================================================================
%% Supervisor - internal 
%%====================================================================

start(_StartType, _StartArgs) ->
    powerhouse_sup:start_link(),
    gen_server:start_link({local, powerhouse}, ?MODULE, [], []).

init([]) ->
    L = supervisor:which_children(powerhouse_sup),
    windmills_on(),
    collect_energy(),
    {ok, {0, [X || {_, X, _, _} <- L]}}. 

%%--------------------------------------------------------------------

handle_info({producer, Energy}, {E, S}) ->
    {noreply, {Energy, S}}.

handle_cast(start, {E, S}) ->
    start_windmills(S),
    {noreply, {E, S}};

handle_cast(stop, {E, S}) ->
    stop_windmills(S),
    {noreply, {E, S}}.

handle_call(user_friendly_state, _From, {E, S}) ->
    {reply, {E, lists:flatlength(S)}, {E,S}};

handle_call({send_energy, Amount}, _From, {E, S}) ->
    if 
        Amount > E ->
            {reply, E, {0, S}};
        true -> 
            {reply, Amount, {E - Amount, S}}
    end;

handle_call(state, _From, {E, S}) ->
    {reply, {E, S}, {E, S}}.
