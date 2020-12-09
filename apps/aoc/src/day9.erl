-module(day9).

%%%_* Exports ==================================================================

-export([ part_1/0
        , part_2/0
        , input/0
        ]).

%%%_* Exported =================================================================

-spec part_1() -> integer().
part_1() ->
  {L1, L2} = lists:split(25, input()),
  solve_part1(L2, L1).

-spec part_2() -> integer().
part_2() ->
  Input = input(),
  IndexedMap = maps:from_list(lists:zip(lists:seq(1,length(Input)), Input)),
  find_range(IndexedMap, part_1(), {{1, 2}, maps:get(1, IndexedMap)}).

%%%_* Internal =================================================================
input() ->
  util:read_file("day9.txt", <<"\n">>, fun(B) -> binary_to_integer(B) end).

solve_part1([H | T], L) ->
  case is_weakness(H, lists:reverse([H | L])) of
    false -> H;
    true -> solve_part1(T, tl(L ++ [H]))
  end.

is_weakness(Int, L) ->
  [ true || X <- L, Y <- L, X + Y =:= Int ] =/= [].

find_range(Map, Answer, {{LIdx, HIdx}, Acc}) when Acc =:= Answer ->
  lists:sum(
    maps:fold(fun(K, V, [Min, Max]) when K >= LIdx andalso K =< HIdx - 1 ->
                  [lists:min([Min, V]), lists:max([Max, V])];
                 (_, _, A) ->
                  A
              end, [infinity, 0], Map));
find_range(Map, Answer, {{LIdx, HIdx}, Acc}) when Acc > Answer ->
  find_range(Map, Answer, {{LIdx + 1, HIdx}, Acc - maps:get(LIdx, Map)});
find_range(Map, Answer, {{LIdx, HIdx}, Acc}) ->
  find_range(Map, Answer, {{LIdx, HIdx + 1}, maps:get(HIdx, Map) + Acc}).

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
