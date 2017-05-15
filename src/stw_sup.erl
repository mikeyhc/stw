%%%-------------------------------------------------------------------
%% @doc stw top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(stw_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

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
    Children = [#{id => stw_server,
                  start =>  {stw_server, start_link, []}
                 }],
    % TODO: work out a suitable supervisor configuration
    {ok, {{one_for_all, 0, 1}, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
