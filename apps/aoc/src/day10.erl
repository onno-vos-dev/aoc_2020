-module(day10).

%%%_* Exports ==================================================================

-export([ part_1/0
        , part_2/0
        ]).

%%%_* Exported =================================================================

-spec part_1() -> integer().
part_1() ->
  AccF = fun(A, B, {X, Y}) when hd(B) - A =:= 1 ->
             {X + 1, Y};
            (A, B, {X, Y}) when hd(B) - A =:= 3 ->
             {X, Y + 1};
            (_, _, {X, Y}) ->
             {X, Y}
         end,
  {X, Y} = find(lists:sort(input()), AccF, {1, 1}),
  X * Y.

-spec part_2() -> integer().
part_2() ->
  AccF = fun(A, B, {I, Acc}) when hd(B) - A =:= 1 ->
             {I + 1, Acc};
            (_, _, {I, Acc}) ->
             {0, maps:update_with(I, fun(V) -> V + 1 end, 1, Acc)}
         end,
  {_, Acc} = find(lists:sort(input()), AccF, {1, #{}}),
  {TwoOnes, ThreeOnes, FourOnes} = {maps:get(2, Acc), maps:get(3, Acc), maps:get(4, Acc)},
  round(math:pow(2, TwoOnes) * math:pow(4, ThreeOnes) * math:pow(7, FourOnes)).

find([], _AccF, Acc) -> Acc;
find([A|T], AccF, Acc) ->
  find(T, AccF, AccF(A, T, Acc)).

input() ->
  util:read_file("day10.txt", <<"\n">>, fun(B) -> binary_to_integer(B) end).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
