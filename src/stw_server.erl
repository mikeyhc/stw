%%%-------------------------------------------------------------------
%% @doc stw main server
%% This server contains most the logic for stw
%% @end
%%%-------------------------------------------------------------------

-module(stw_server).

-behaviour(gen_server).

-include("stw_types.hrl").
-include("stw_macros.hrl").

%% public api
-export([start_link/0]).
-export([add_entry/2, list_entries/1]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%%=====================================================================
%% API
%%=====================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

add_entry(Ref, Entry) ->
    gen_server:call(Ref, {add_entry, Entry}).

list_entries(Ref) ->
    gen_server:call(Ref, list_entries).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) -> {ok, #{}}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extras) -> {ok, State}.

handle_call({add_entry, Entry}, _From, State) ->
    {reply, add_entry(Entry), State};
handle_call(list_entries, _From, State) ->
    {reply, list_entries(), State}.

handle_cast(_Message, State) -> {noreply, State}.

handle_info(_Message, State) -> {noreply, State}.

%%====================================================================
%% Internal functions
%%====================================================================

add_entry(Entry) ->
    F = fun() ->
        case mnesia:read({stw_entry, Entry#stw_entry.title}) =:= [] of
            false -> {error, already_exists};
            true -> mnesia:write(Entry)
        end
    end,
    mnesia:activity(transaction, F).

list_entries() ->
    F = fun() -> mnesia:foldl(fun(X, Y) -> [X|Y] end, [], stw_entry) end,
    MapFun = ?make_record_to_map(stw_entry),
    lists:map(MapFun,
              lists:sort(fun(X, Y) -> X#stw_entry.date > Y#stw_entry.date end,
                         mnesia:activity(transaction, F))).
