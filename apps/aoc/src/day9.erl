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
  find_range(input(), 2, part_1()).

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

find_range(L, N, Answer) ->
  case find_range_n(L, N, Answer) of
    false -> find_range(L, N + 1, Answer);
    {true, Result} -> lists:min(Result) + lists:max(Result)
  end.

find_range_n(L, N, _) when length(L) < N -> false;
find_range_n(L, N, Answer) ->
  {L1, _} = lists:split(N, L),
  case lists:sum(L1) of
    Answer -> {true, L1};
    Sum when Sum > Answer -> false;
    _ -> find_range_n(tl(L), N, Answer)
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
