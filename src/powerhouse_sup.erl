%%%-------------------------------------------------------------------
%% @doc Powerhouse supervisor
%% @end
%%%-------------------------------------------------------------------

-module(powerhouse_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, windmills/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_one, 0, 1}, windmills()} }.

%%====================================================================
%% Internal functions
%%====================================================================

windmills() ->
    [#{id => X, start => {windmill, start_link, [{0, 0}]}} || X <- lists:seq(1,10)].
