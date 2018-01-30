%%%-------------------------------------------------------------------
%% @doc elektrownia public API.
%% @end
%%%-------------------------------------------------------------------

-module(client).

%% Application callbacks
-compile([export_all]).

%%====================================================================
%% API
%%====================================================================

create() ->
    spawn(client, greedy_consumer, []).

%%====================================================================
%% Client - internal
%%====================================================================

greedy_consumer() ->
    Amount = get_amount(),
    powerhouse:consume_energy(Amount),
    timer:sleep(2000),
    greedy_consumer().

get_amount() ->
    rand:uniform()*10.
