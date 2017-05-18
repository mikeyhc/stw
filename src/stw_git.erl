%%%-------------------------------------------------------------------
%% @doc stw git helpers
%% @end
%%%-------------------------------------------------------------------

-module(stw_git).

-include("stw_types.hrl").

-export([with_repository/2, read_entry/2]).

with_repository(Path, Fun) ->
    {ok, OldDir} = file:get_cwd(),
    try
        file:set_cwd(Path),
        Fun()
    after
        file:set_cwd(OldDir)
    end.

read_git_date(File) ->
    Date = string:strip(os:cmd(["git log --format=%aD ", File, " | tail -1"]),
                        right, $\n),
    qdate:to_date(Date).

read_git_author(File) ->
    Cmd = ["git log --format=%aN ", File, " | tail -1"],
    binary:list_to_bin(string:strip(os:cmd(Cmd), right, $\n)).

unwrap({ok, Value}) -> Value.

-spec read_entry(stw_path(), stw_path()) -> stw_entry().
read_entry(Repo, Path) ->
    with_repository(Repo, fun() ->
        {ok, IODevice} = file:open(Path, [read]),
        Title = string:strip(unwrap(file:read_line(IODevice)), right, $\n),
        file:close(IODevice),
        Date = read_git_date(Path),
        Author = read_git_author(Path),
        #stw_entry{title = binary:list_to_bin(Title),
                   date = Date,
                   author = Author,
                   path = Path}
    end).
