%%%-------------------------------------------------------------------
%% @doc stw public api
%% @end
%%%-------------------------------------------------------------------

-module(stw).

-include("stw_types.hrl").

-export([create_cluster/1, destroy_cluster/1, add_entry/1]).

-define(SUP_REF, stw_sup).
-define(CHILD_REF, stw_server).

create_cluster(Nodes) ->
    mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:create_table(stw_entry,
                        [{attributes, record_info(fields, stw_entry)},
                          {disc_copies, Nodes}]),
    rpc:multicall(Nodes, application, stop, [mnesia]).

destroy_cluster(Nodes) ->
    rpc:multicall(Nodes, application, stop, [mnesia]),
    mnesia:delete_schema([Nodes]).

add_entry(Entry) ->
    stw_server:add_entry(server_pid(), Entry).

%%====================================================================
%% Internal functions
%%====================================================================

server_pid() ->
    Children = supervisor:which_children(?SUP_REF),
    {_Id, Pid, _Type, _Modules} = lists:keyfind(?CHILD_REF, 1, Children),
    Pid.
