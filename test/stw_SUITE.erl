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
-export([add_git_entry/1]).

-define(FIRST_ENTRY, #stw_entry{title = <<"Blog Entry 1">>,
                                date = {{2017, 5, 15}, {10, 38, 11}},
                                author = <<"mikeyhc">>,
                                path = <<"priv/test_blog/entry1.md">>}).
-define(SECOND_ENTRY, #stw_entry{title = <<"Blog Entry 2">>,
                                 date = {{2017, 5, 16}, {9, 34, 23}},
                                 author = <<"mikeyhc">>,
                                 path = <<"priv/test_blog/entry2.md">>}).

-define(BLOG_MIN_TITLE, 8).
-define(BLOG_MAX_TITLE, 20).

-define(BLOG_MIN_LENGTH, 1500).
-define(BLOG_MAX_LENGTH, 2000).

-define(LINE_LENGTH, 80).

all() -> [add_entry,
          list_entries,
          add_git_entry
         ].

init_per_suite(Config)  ->
    Priv = ?config(priv_dir, Config),
    application:set_env(mnesia, dir, Priv),
    stw:create_cluster([node()]),
    application:ensure_all_started(stw),
    Path = string:strip(os:cmd("mktemp -d"), right, $\n),
    Entries = stw_git:with_repository(Path, fun() ->
        GitEntry1 = create_blog_entry(Path),
        GitEntry2 = create_blog_entry(Path),
        os:cmd("git init " ++ Path),
        lists:foreach(fun(Cmd) ->
                            io:format("running command:   ~s~n", [Cmd]),
                            os:cmd(Cmd)
                      end,
                      ["git init",
                       "git config --local user.name testuser",
                       "git config --local user.email test@atmosia.net",
                       "git add " ++ GitEntry1,
                       "git commit -m \"initial commit\"",
                       "git add " ++ GitEntry2,
                       "git commit -m \"second entry\""
                      ]),
        [GitEntry1, GitEntry2]
    end),
    [{repodir, Path}, {git_entries, Entries}|Config].

end_per_suite(Config) ->
    os:cmd("rm -rf " ++ ?config(repodir, Config)),
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

add_git_entry(Config) ->
    Repo = ?config(repodir, Config),
    lists:foreach(fun(Path) -> ok = stw:add_git_entry(Repo, Path) end,
                  ?config(git_entries, Config)),
    Path = hd(?config(git_entries, Config)),
    {error, already_exists} = stw:add_git_entry(Repo, Path).

%%====================================================================
%% Internal functions
%%====================================================================

take(N, L) -> take(N, L, []).

take(0, R, Acc) -> {lists:reverse(Acc), R};
take(_N, [], Acc) -> {lists:reverse(Acc),[]};
take(N, [H|T], Acc) ->
    take(N - 1, T, [H|Acc]).

add_newlines([]) -> [];
add_newlines(Body) ->
    {L, R} = take(?LINE_LENGTH, Body),
    L ++ [$\n|add_newlines(R)].

rand_lower() -> lists:seq($a, $z).
rand_upper() -> lists:seq($A, $Z).
rand_num() -> lists:seq($0, $9).
% make spaces more common to create "words"
rand_space() -> lists:map(fun(_) -> $  end, lists:seq(1, 10)).

pick_rand(Set) ->
    Index = round(rand:uniform() * (length(Set) - 1)),
    lists:nth(Index + 1, Set).

randchar() ->
    pick_rand(rand_space() ++ rand_lower() ++ rand_upper() ++ rand_num()).

create_blog_entry(Path) ->
    Template = Path ++ "/blog.XXXXXX",
    FilePath = string:strip(os:cmd("mktemp " ++ Template), right, $\n),
    {ok, Handle} = file:open(FilePath, [write]),
    TitleVariance = ?BLOG_MAX_TITLE - ?BLOG_MIN_TITLE,
    TitleLength = ?BLOG_MIN_TITLE + round(rand:uniform() * TitleVariance),
    TitleBuilder = fun(_, {L, R}) -> {[randchar()|L], [$=|R]} end,
    {Title, Underline} = lists:foldl(TitleBuilder, {[], []},
                                     lists:seq(1, TitleLength)),
    BodyVariance = ?BLOG_MAX_LENGTH - ?BLOG_MIN_LENGTH,
    BodyLength = ?BLOG_MIN_LENGTH + round(rand:uniform() * BodyVariance),
    Body = add_newlines(lists:foldl(fun(_, L) -> [randchar()|L] end, [],
                                    lists:seq(1, BodyLength))),
    io:format(Handle, "~s~n~s~n~n~s", [Title, Underline, Body]),
    file:close(Handle),
    filename:basename(FilePath).
