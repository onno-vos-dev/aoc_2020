-module(day1).

%%%_* Exports ==================================================================

-export([ part_1/0
        , part_2/0
        ]).

%%%_* Exported =================================================================

-spec part_1() -> integer().
part_1() ->
  solve(input(), part_1_fun(), []).

-spec part_2() -> integer().
part_2() ->
  solve(input(), part_2_fun(), []).

%%%_* Internal =================================================================
input() ->
  util:read_file("day1.txt", <<"\n">>, fun erlang:binary_to_integer/1).

solve([H | T], Fun, Acc) when is_function(Fun, 2) ->
  case Fun(H, T) of
    [] -> solve(T, Fun, Acc);
    Res -> hd(Res)
  end.

part_1_fun() ->
  fun(X, List) ->
      lists:flatmap(fun(I) when X + I =:= 2020 -> [X * I];
                       (_) -> []
                    end, List)
  end.

part_2_fun() ->
  fun(X, List) ->
      lists:flatmap(
        fun(Y) ->
            lists:flatmap(
              fun(Z) when X + Y + Z =:= 2020 -> [X * Y * Z];
                 (_) -> []
              end, List -- [Y])
        end, List)
  end.

%%%_* EUnit ====================================================================
-ifdef(EUNIT).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").

day1a_example_test() ->
  ?assertEqual(514579,
               solve([1721, 979, 366, 299, 675, 1456], part_1_fun(), [])).

day1b_example_test() ->
  ?assertEqual(241861950,
               solve([1721, 979, 366, 299, 675, 1456], part_2_fun(), [])).

-endif.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
