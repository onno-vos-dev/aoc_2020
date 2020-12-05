-module(day5).

%%%_* Exports ==================================================================

-export([ part_1/0
        , part_2/0
        ]).

%%%_* Exported =================================================================

-spec part_1() -> integer().
part_1() ->
  lists:max(solve(input(), [])).

-spec part_2() -> integer().
part_2() ->
  find_seat(lists:sort(solve(input(), []))).

%%%_* Internal =================================================================
input() ->
  util:read_file("day5.txt", <<"\n">>, fun(A) -> A end).

solve([], Acc) -> Acc;
solve([H | T], Acc) ->
  solve(T, [ calc_seat_id(H) | Acc]).

calc_seat_id(H) ->
  {{Row,_}, {Seat,_}} =
    lists:foldl(fun($F, {{MinRow, MaxRow}, {MinSeat, MaxSeat}}) ->
                    {{MinRow, (MinRow + MaxRow) div 2}, {MinSeat, MaxSeat}};
                   ($B, {{MinRow, MaxRow}, {MinSeat, MaxSeat}}) ->
                    {{(MinRow + MaxRow) div 2, MaxRow}, {MinSeat, MaxSeat}};
                   ($R, {{MinRow, MaxRow}, {MinSeat, MaxSeat}}) ->
                    {{MinRow, MaxRow}, {(MinSeat + MaxSeat) div 2, MaxSeat}};
                   ($L, {{MinRow, MaxRow}, {MinSeat, MaxSeat}}) ->
                    {{MinRow, MaxRow}, {MinSeat, (MinSeat + MaxSeat) div 2}}
                end, {{0, 128}, {0, 8}}, binary_to_list(H)),
  (Row * 8) + Seat.

find_seat([A, B| _]) when B =:= A + 2 -> A + 1;
find_seat([_ | T]) -> find_seat(T).

%%%_* EUnit ====================================================================
-ifdef(EUNIT).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").

calc_seat_id_test() ->
  ?assertEqual(567, calc_seat_id(<<"BFFFBBFRRR">>)),
  ?assertEqual(119, calc_seat_id(<<"FFFBBBFRRR">>)),
  ?assertEqual(820, calc_seat_id(<<"BBFFBBFRLL">>)).

-endif.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
