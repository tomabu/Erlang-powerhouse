%%%-------------------------------------------------------------------
%% @doc Windmill.
%% @end
%%%-------------------------------------------------------------------

-module(windmill).
-behaviour(gen_server).

%% API
-compile([export_all]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Odblokowuje wiatrak o danym Pidzie
start_rotating(Pid) ->
    Controller_pid = spawn(windmill, produce, [Pid]),
    gen_server:call(Pid, {windmill_started, Controller_pid}).

%% @doc Blokuje wiatrak o podanym Pidzie
stop_rotating(Pid) ->
    gen_server:call(Pid, windmill_stoped).

%% @doc Wyświetla ilość aktualnie produkowanej energii
get_energy(Pid) ->
    gen_server:call(Pid, state).

%%====================================================================
%% Windmill - internal
%%====================================================================

%% @doc Funkcja pętla służąca do wysyłania komunikatów do procesu wiatraka, aby ten zmieniał (czyli niejako produkował) swoją energię
produce(Pid) ->
    Pid ! 'Start',
    timer:sleep(2000),
    produce(Pid). 

%% @doc Funkcja według której zmienia się produkowana przez wiatrak energia
produce_energy({_, Pid}) ->
    {rand:uniform()*10, Pid}.

%%====================================================================
%% GenServer - Internal
%%====================================================================

start_link(InitState) ->
    gen_server:start_link(?MODULE, InitState, []).

stop(Pid) ->
    gen_server:stop(Pid).

init(S) ->
    {ok, S}.

%%--------------------------------------------------------------------

handle_info('Start', S) ->
    NewState = produce_energy(S),
    {noreply, NewState}.

%% @doc Obsługa włączenia
handle_call({windmill_started, Pid}, _From, {Energy, _}) ->
    {reply, "Windmill started", {Energy, Pid}};

%% @doc Obsługa zatrzymania, zabija proces kontrolujący zmianę wartości produkowanej energii
handle_call(windmill_stoped, _From, {_, Pid}) ->
    exit(Pid, kill),
    {reply, "Windmill stopped", {0, 0}};

handle_call(state, _From, {Energy, Pid}) ->
    {reply, Energy, {Energy, Pid}}.