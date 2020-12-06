-module(day6).

%%%_* Exports ==================================================================

-export([ part_1/0
        , part_2/0
        ]).

%%%_* Exported =================================================================

-spec part_1() -> integer().
part_1() ->
  solve(input(), solve_fun(fun(Acc, Set) -> sets:union(Acc, Set) end), 0).

-spec part_2() -> integer().
part_2() ->
  solve(input(), solve_fun(fun(Acc, Set) -> sets:intersection(Acc, Set) end), 0).

%%%_* Internal =================================================================
input() ->
  util:read_file("day6.txt", <<"\n\n">>, fun(B) -> binary:split(B, <<"\n">>, [trim, global]) end).

solve([], _Fun, Acc) -> Acc;
solve([H | T], Fun, Acc) ->
  solve(T, Fun, Fun(H) + Acc).

solve_fun(F) ->
  fun(Bins) ->
      sets:size(
        lists:foldl(
          fun(Bin, Acc) ->
              F(Acc, sets:from_list(binary_to_list(Bin)))
          end, sets:from_list(binary_to_list(hd(Bins))), tl(Bins)))
  end.

%%%_* EUnit ====================================================================
-ifdef(EUNIT).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").

%% TODO

-endif.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
