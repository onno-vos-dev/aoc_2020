-module(day9).

%%%_* Exports ==================================================================

-export([ part_1/0
        , part_2/0
        , input/0
        ]).

%%%_* Exported =================================================================

-spec part_1() -> integer().
part_1() ->
  Preamble = 25,
  {L1, L2} = lists:split(Preamble, input()),
  solve_part1(L2, L1, Preamble, [ X + Y || X <- L1, Y <- L1]).

-spec part_2() -> integer().
part_2() ->
  Input = input(),
  IndexedMap = maps:from_list(lists:zip(lists:seq(1,length(Input)), Input)),
  find_range(IndexedMap, part_1(), {{1, 2}, maps:get(1, IndexedMap), [maps:get(1, IndexedMap)]}).

%%%_* Internal =================================================================
input() ->
  util:read_file("day9.txt", <<"\n">>, fun(B) -> binary_to_integer(B) end).

solve_part1([H | T], L, Preamble, Old) ->
  New = calculate_possible_sums(H, L),
  case lists:member(H, New ++ Old) of
    false -> H;
    true -> solve_part1(T, tl(L ++ [H]), Preamble, new_possible_sums(Preamble, Old, New))
  end.

calculate_possible_sums(Int, L) ->
  [ X + Y || X <- L, Y <- [Int]].

new_possible_sums(Preamble, Old, New) ->
  element(2, lists:split(Preamble, Old)) ++ New.

find_range(_Map, Answer, {{_LIdx, _HIdx}, Acc, MinMax}) when Acc =:= Answer ->
  lists:min(MinMax) + lists:max(MinMax);
find_range(Map, Answer, {{LIdx, HIdx}, Acc, MinMax}) when Acc > Answer ->
  V = maps:get(LIdx, Map),
  find_range(Map, Answer, {{LIdx + 1, HIdx}, Acc - V, MinMax -- [V]});
find_range(Map, Answer, {{LIdx, HIdx}, Acc, MinMax}) ->
  V = maps:get(HIdx, Map),
  find_range(Map, Answer, {{LIdx, HIdx + 1}, V + Acc, [V | MinMax]}).

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
