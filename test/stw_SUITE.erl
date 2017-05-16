%%%-------------------------------------------------------------------
%% @doc stw tests
%% @end
%%%-------------------------------------------------------------------

-module(stw_SUITE).

-include("stw_types.hrl").
-include("stw_macros.hrl").
-include_lib("common_test/include/ct.hrl").

-export([init_per_suite/1, end_per_suite/1, all/0]).
-export([add_entry/1, list_entries/1]).

-define(FIRST_ENTRY, #stw_entry{title = <<"Blog Entry 1">>,
                                date = {{2017, 5, 15}, {10, 38, 11}},
                                author = <<"mikeyhc">>,
                                path = <<"priv/test_blog/entry1.md">>}).
-define(SECOND_ENTRY, #stw_entry{title = <<"Blog Entry 2">>,
                                 date = {{2017, 5, 16}, {9, 34, 23}},
                                 author = <<"mikeyhc">>,
                                 path = <<"priv/test_blog/entry2.md">>}).

all() -> [add_entry,
          list_entries
         ].

init_per_suite(Config)  ->
    Priv = ?config(priv_dir, Config),
    application:set_env(mnesia, dir, Priv),
    stw:create_cluster([node()]),
    application:ensure_all_started(stw),
    Config.

end_per_suite(_Config) ->
    stw:destroy_cluster([node()]),
    application:stop(stw),
    application:stop(mnesia).

add_entry(_Config) ->
    Entry = ?FIRST_ENTRY,
    ok = stw:add_entry(Entry#stw_entry.title,
                       Entry#stw_entry.date,
                       Entry#stw_entry.author,
                       Entry#stw_entry.path),
    {error, already_exists} = stw:add_entry(Entry#stw_entry.title,
                                            Entry#stw_entry.date,
                                            Entry#stw_entry.author,
                                            Entry#stw_entry.path).

list_entries(_Config) ->
    FEntry = ?FIRST_ENTRY,
    stw:add_entry(FEntry#stw_entry.title,
                  FEntry#stw_entry.date,
                  FEntry#stw_entry.author,
                  FEntry#stw_entry.path),
    SEntry = ?SECOND_ENTRY,
    stw:add_entry(SEntry#stw_entry.title,
                  SEntry#stw_entry.date,
                  SEntry#stw_entry.author,
                  SEntry#stw_entry.path),
    MapFun = ?make_record_to_map(stw_entry),
    Output = [MapFun(?SECOND_ENTRY), MapFun(?FIRST_ENTRY)],
    Output = stw:list_entries().
