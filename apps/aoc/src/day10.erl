-module(day10).

%%%_* Exports ==================================================================

-export([ part_1/0
        , part_2/0
        , golf/1
        ]).

%%%_* API ======================================================================
-spec golf(string()) -> {integer(), integer()}.
golf(File) ->
  {ok, Bin} = file:read_file(File),
  Input = lists:sort([ binary_to_integer(S) || S <- binary:split(Bin, <<"\n">>, [trim, global]) ]),
  FindF = fun(_F, [], _AccF, Acc) -> Acc;
             (F, [A|T], AccF, Acc) ->
              F(F, T, AccF, AccF(A, T, Acc))
          end,
  MapF = fun(Acc, I) ->
             case Acc of
               #{I := V} -> Acc#{I := V + 1};
               #{} -> Acc#{I => 1}
             end
         end,
  AccF1 = fun(A, B, {{X, Y}, {I, Acc}}) when hd(B) - A =:= 1 ->
              {{X + 1, Y}, {I + 1, Acc}};
             (A, B, {{X, Y}, {I, Acc}}) when hd(B) - A =:= 3 ->
              {{X, Y + 1}, {0, MapF(Acc, I)}};
             (_, _, {{X, Y}, {I, Acc}}) ->
              {{X, Y}, {0, MapF(Acc, I)}}
          end,
  {{X, Y}, {_, #{2 := Twos, 3 := Threes, 4:= Fours}}} = FindF(FindF, Input, AccF1, {{1, 1}, {1, #{}}}),
  A = X * Y,
  B = round(math:pow(2, Twos) * math:pow(4, Threes) * math:pow(7, Fours)),
  {A, B}.

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

%%%_* Internal =================================================================
-compile({inline, [find/3]}).
find([], _AccF, Acc) -> Acc;
find([A|T], AccF, Acc) ->
  find(T, AccF, AccF(A, T, Acc)).

input() ->
  util:read_file("day10.txt", <<"\n">>, fun erlang:binary_to_integer/1).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
