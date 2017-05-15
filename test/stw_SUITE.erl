%%%-------------------------------------------------------------------
%% @doc stw tests
%% @end
%%%-------------------------------------------------------------------

-module(stw_SUITE).

-include("stw_types.hrl").
-include_lib("common_test/include/ct.hrl").

-export([init_per_suite/1, end_per_suite/1, all/0]).
-export([add_entry/1]).

-define(TEST_ENTRY, #stw_entry{title = <<"Blog Entry 1">>,
                               date = {{2017, 5, 15}, {10, 38, 11}},
                               author = <<"mikeyhc">>,
                               path = <<"priv/test_blog/entry1.md">>
                              }).

all() -> [add_entry].

init_per_suite(Config)  ->
    Priv = ?config(priv_dir, Config),
    application:set_env(mnesia, dir, Priv),
    stw:create_cluster([node()]),
    application:ensure_all_started(stw),
    Config.

end_per_suite(_Config) ->
    stw:destroy_cluster([node()]),
    application:stop(mnesia).

add_entry(_Config) ->
    ok = stw:add_entry(?TEST_ENTRY),
    {error, already_exists} = stw:add_entry(?TEST_ENTRY).
