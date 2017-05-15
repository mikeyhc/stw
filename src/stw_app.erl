%%%-------------------------------------------------------------------
%% @doc stw application
%% @end
%%%-------------------------------------------------------------------

-module(stw_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    mnesia:wait_for_tables([stw_entry], 5000),
    stw_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
