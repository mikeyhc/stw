%%%-------------------------------------------------------------------
%% @doc stw public api
%% @end
%%%-------------------------------------------------------------------

-module(stw).

-include("stw_types.hrl").

-export([create_cluster/1, destroy_cluster/1, add_entry/4, list_entries/0]).
-export([add_git_entry/2]).

-define(SUP_REF, stw_sup).
-define(CHILD_REF, stw_server).

-spec create_cluster([node()]) -> ok.
create_cluster(Nodes) ->
    mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:create_table(stw_entry,
                        [{attributes, record_info(fields, stw_entry)},
                          {disc_copies, Nodes}]),
    rpc:multicall(Nodes, application, stop, [mnesia]),
    ok.

-spec destroy_cluster([node()]) -> ok.
destroy_cluster(Nodes) ->
    rpc:multicall(Nodes, application, stop, [mnesia]),
    mnesia:delete_schema([Nodes]),
    ok.

-spec add_entry(binary(), calendar:datetime(), binary(), binary()) ->
    ok | {error, already_exists}.
add_entry(Title, Date, Author, Path) ->
    Entry = #stw_entry{title = Title,
                       date = Date,
                       author = Author,
                       path = Path},
    stw_server:add_entry(server_pid(), Entry).

-spec list_entries() -> [map()].
list_entries() ->
    stw_server:list_entries(server_pid()).

-spec add_git_entry(stw_path(), stw_path()) -> ok | {error, already_exists}.
add_git_entry(Repo, Path) ->
    Entry = stw_git:read_entry(Repo, Path),
    stw_server:add_entry(server_pid(), Entry).

%%====================================================================
%% Internal functions
%%====================================================================

server_pid() ->
    Children = supervisor:which_children(?SUP_REF),
    {_Id, Pid, _Type, _Modules} = lists:keyfind(?CHILD_REF, 1, Children),
    Pid.
