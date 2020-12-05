-module(day3).

%%%_* Exports ==================================================================

-export([ part_1/0
        , part_2/0
        ]).

%%%_* Exported =================================================================

-spec part_1() -> integer().
part_1() ->
  Start = 4,
  Right = 3,
  Down = 1,
  solve(tl(input()), {{Start, Right, Down}, 0}).

-spec part_2() -> integer().
part_2() ->
  One = solve(tl(input()), {{2, 1, 1}, 0}),
  Two = solve(tl(input()), {{4, 3, 1}, 0}),
  Three = solve(tl(input()), {{6, 5, 1}, 0}),
  Four = solve(tl(input()), {{8, 7, 1}, 0}),
  Five = solve(tl(input()), {{2, 1, 2}, 0}),
  One * Two * Three * Four * Five.

%%%_* Internal =================================================================
input() ->
  util:read_file("day3.txt", <<"\n">>, fun erlang:binary_to_list/1).

solve([], {_, Acc}) -> Acc;
solve([H | T], {{Pos, Right, Down}, Acc}) ->
  Fun = tree_fun(),
  case Down of
    1 -> solve(T, {{Pos + Right, Right, Down}, Fun(Fun, H, Pos) + Acc});
    2 -> solve(tl(T), {{Pos + Right, Right, Down}, Fun(Fun, hd(T), Pos) + Acc})
  end.

tree_fun() ->
  fun(Fun, Line, I) ->
      case length(Line) >= I of
        true ->
          case lists:nth(I, Line) of
            $# -> 1;
            $\. -> 0
          end;
        false ->
          Fun(Fun, Line ++ Line, I)
      end
  end.

%%%_* EUnit ====================================================================
-ifdef(EUNIT).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").

day1a_example_test() ->
  Input = util:read_file("day3_example.txt", <<"\n">>, fun erlang:binary_to_list/1),
  ?assertEqual(7, solve(tl(Input), {{4, 3, 1}, 0})).

day1b_example_test() ->
  Input = util:read_file("day3_example.txt", <<"\n">>, fun erlang:binary_to_list/1),
  One = solve(tl(Input), {{2, 1, 1}, 0}),
  Two = solve(tl(Input), {{4, 3, 1}, 0}),
  Three = solve(tl(Input), {{6, 5, 1}, 0}),
  Four = solve(tl(Input), {{8, 7, 1}, 0}),
  Five = solve(tl(Input), {{2, 1, 2}, 0}),
  ?assertEqual(336, One * Two * Three * Four * Five).

-endif.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
