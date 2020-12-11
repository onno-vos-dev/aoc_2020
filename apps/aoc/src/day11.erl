-module(day11).

%%%_* Exports ==================================================================

-export([ part_1/0
        , part_2/0
        ]).

%%%_* API ======================================================================

-spec part_1() -> integer().
part_1() ->
  Input0 = input(),
  Input1 = lists:zip(lists:seq(1, length(Input0)), Input0),
  Input = lists:map(fun({Num, Seats}) -> {Num, false, Seats} end, Input1),
  calculate_occupancy(Input, [], Input, part_1_map_fun()).

-spec part_2() -> integer().
part_2() ->
  Input0 = input(),
  Input1 = lists:zip(lists:seq(1, length(Input0)), Input0),
  Input = lists:map(fun({Num, Seats}) -> {Num, false, Seats} end, Input1),
  R = calculate_occupancy(Input, [], Input, part_2_map_fun()),
  R.

%%%_* Internal =================================================================
input() ->
  util:read_file("day11.txt",
                 <<"\n">>,
                 fun(A) ->
                     maps:from_list(lists:zip(lists:seq(1, length(binary_to_list(A))), binary_to_list(A)))
                 end).

calculate_occupancy([], Acc, Acc, _) ->
  lists:foldl(fun({_, _, M}, N) ->
                  maps:fold(fun(_, $#, A) -> A + 1; (_, _, A) -> A end, N, M)
              end, 0, Acc);
calculate_occupancy([], NewAcc, OldAcc, F) when NewAcc =/= OldAcc ->
  calculate_occupancy(NewAcc, [], NewAcc, F);
calculate_occupancy([{RowNum, true, SeatsM} | T], Acc, OldAcc, F) ->
  calculate_occupancy(T, lists:sort([{RowNum, true, SeatsM} | Acc]), OldAcc, F);
calculate_occupancy([{RowNum, false, SeatsM} | T], Acc, OldAcc, F) ->
  case {lists:keyfind(RowNum - 1, 1, OldAcc), lists:keyfind(RowNum + 1, 1, T)} of
    {false, {_, _, NextRow}} ->
      NewSeatsM = maybe_change_seats({RowNum, SeatsM}, #{}, NextRow, F, OldAcc),
      calculate_occupancy(T, lists:sort([{RowNum, NewSeatsM =:= SeatsM, NewSeatsM} | Acc]), OldAcc, F);
    {{_, _, PreviousRow}, {_, _, NextRow}} ->
      NewSeatsM = maybe_change_seats({RowNum, SeatsM}, PreviousRow, NextRow, F, OldAcc),
      calculate_occupancy(T, lists:sort([{RowNum, NewSeatsM =:= SeatsM, NewSeatsM} | Acc]), OldAcc, F);
    {{_, _, PreviousRow}, false} ->
      NewSeatsM = maybe_change_seats({RowNum, SeatsM}, PreviousRow, #{}, F, OldAcc),
      calculate_occupancy(T, lists:sort([{RowNum, NewSeatsM =:= SeatsM, NewSeatsM} | Acc]), OldAcc, F)
  end.

maybe_change_seats({RowNum, SeatsM}, PreviousRow, NextRow, F, AllRows) ->
  maps:fold(fun(K, V, Acc) ->
                F(K, V, Acc, {RowNum, SeatsM}, PreviousRow, NextRow, AllRows)
            end, #{}, SeatsM).

part_1_map_fun() ->
  fun(K, V, Acc, {_, SeatsM}, PreviousRow, NextRow, _AllRows) ->
      Seats = surrounding_seats(K, SeatsM, PreviousRow, NextRow),
      OccupiedSeats = lists:filter(fun($#) -> true; (_) -> false end, Seats),
      case {V, length(OccupiedSeats)} of
        {$#, Length} when Length >= 4 ->
          maps:put(K, $L, Acc);
        {$L, 0} ->
          maps:put(K, $#, Acc);
        _ ->
          maps:put(K, V, Acc)
      end
  end.

surrounding_seats(K, CurrentRow, PreviousRow, NextRow) ->
  Left        = maps:get(K - 1, CurrentRow, []),
  Right       = maps:get(K + 1, CurrentRow, []),
  Top         = maps:get(K,     PreviousRow, []),
  TopLeft     = maps:get(K - 1, PreviousRow, []),
  TopRight    = maps:get(K + 1, PreviousRow, []),
  Bottom      = maps:get(K,     NextRow, []),
  BottomLeft  = maps:get(K - 1, NextRow, []),
  BottomRight = maps:get(K + 1, NextRow, []),
  [Left, Right, Top, Bottom, BottomLeft, BottomRight, TopLeft, TopRight].

part_2_map_fun() ->
  fun(K, V, Acc, {RowNum, _SeatsM}, _PreviousRow, _NextRow, AllRows) ->
      Seats = visible_seats({K, RowNum}, AllRows),
      OccupiedSeats = lists:filter(fun($#) -> true; (_) -> false end, tuple_to_list(Seats)),
      case {V, length(OccupiedSeats)} of
        {$#, Length} when Length >= 5 ->
          maps:put(K, $L, Acc);
        {$L, 0} ->
          maps:put(K, $#, Acc);
        _ ->
          maps:put(K, V, Acc)
      end
  end.

-record(seats, { left = [],
                 right = [],
                 top = [],
                 top_left = [],
                 top_right = [],
                 bottom = [],
                 bottom_left = [],
                 bottom_right = []
               }).

visible_seats({X, Y}, AllRows) ->
  MaxSize = maps:size(element(3, hd(AllRows))),
  Seats0 = fold_top(lists:reverse(AllRows), #seats{}, {X, Y}),
  Seats1 = fold_bottom(AllRows, Seats0, {X, Y}),
  Seats2 = fold_left(lists:reverse(lists:seq(1, X - 1)), element(3, lists:keyfind(Y, 1, AllRows)), Seats1, {X, Y}),
  fold_right(lists:seq(X + 1, MaxSize), element(3, lists:keyfind(Y, 1, AllRows)), Seats2, {X, Y}).

fold_top([], Seats, _) -> Seats;
fold_top(_, #seats{top = Top, top_left = TopLeft, top_right = TopRight} = Seats, {_, _})
  when Top =/= [] andalso TopLeft =/= [] andalso TopRight =/= [] ->
  Seats;
fold_top([{RowNum, _, Map}|Tail], #seats{} = Seats, {X, Y}) when Y > RowNum ->
  NewSeats0 = set_seats(Seats, X, Y, RowNum, find_seats_or_out_of_bound(X, Y, {RowNum, Map}), tc),
  case maps:get(X, Map) of
    $\. -> fold_top(Tail, NewSeats0, {X, Y});
    Char when NewSeats0#seats.top =:= [] -> fold_top(Tail, NewSeats0#seats{top = Char}, {X, Y});
    _ -> fold_top(Tail, NewSeats0, {X, Y})
  end;
fold_top([_|Tail], Seats, {X, Y}) ->
  fold_top(Tail, Seats, {X, Y}).

fold_bottom([], Seats, _) -> Seats;
fold_bottom(_, #seats{bottom = Bottom, bottom_left = BottomLeft, bottom_right = BottomRight} = Seats, {_, _})
  when Bottom =/= [] andalso BottomLeft =/= [] andalso BottomRight =/= [] ->
  Seats;
fold_bottom([{RowNum, _, Map}|Tail], #seats{} = Seats, {X, Y}) when Y < RowNum ->
  NewSeats0 = set_seats(Seats, X, Y, RowNum, find_seats_or_out_of_bound(X, Y, {RowNum, Map}), bc),
  case maps:get(X, Map) of
    $\. -> fold_bottom(Tail, NewSeats0, {X, Y});
    Char when NewSeats0#seats.bottom =:= [] -> fold_bottom(Tail, NewSeats0#seats{bottom = Char}, {X, Y});
    _ -> fold_bottom(Tail, NewSeats0, {X, Y})
  end;
fold_bottom([_|Tail], Seats, {X, Y}) ->
  fold_bottom(Tail, Seats, {X, Y}).

fold_left([], _, Seats, _) -> Seats;
fold_left(_, _, #seats{left = Left} = Seats, _) when Left =/= [] -> Seats;
fold_left([I|Tail], Map, #seats{} = Seats, {X, Y}) ->
  case maps:get(I, Map) of
    $\. -> fold_left(Tail, Map, Seats, {X, Y});
    Char -> fold_left(Tail, Map, Seats#seats{left = Char}, {X, Y})
  end.

fold_right([], _, Seats, _) -> Seats;
fold_right(_, _, #seats{right = Right} = Seats, _) when Right =/= [] -> Seats;
fold_right([I|Tail], Map, #seats{} = Seats, {X, Y}) ->
  case maps:get(I, Map) of
    $\. -> fold_right(Tail, Map, Seats, {X, Y});
    Char -> fold_right(Tail, Map, Seats#seats{right = Char}, {X, Y})
  end.

find_seats_or_out_of_bound(X, Y, {YM, Map}) ->
  case Y > YM of
    true -> {maps:get(X - (Y - YM), Map, []), maps:get(X + (Y - YM), Map, [])};
    false -> {maps:get(X - (YM - Y), Map, []), maps:get(X + (YM - Y), Map, [])}
  end.

set_seats(Acc, _, _, _, {TopLeft, TopRight}, tc) ->
  case {TopLeft, TopRight} of
    {$\., $\.} -> Acc;
    {$\., Char} when Acc#seats.top_right =:= [] ->
      Acc#seats{top_right = Char};
    {Char, $\.} when Acc#seats.top_left =:= [] ->
      Acc#seats{top_left = Char};
    {LC, RC} when LC =/= $\. andalso RC =/= $\. andalso Acc#seats.top_left =:= [] andalso Acc#seats.top_right =:= [] ->
      Acc#seats{top_left = LC, top_right = RC};
    {_, RC} when RC =/= $\. andalso Acc#seats.top_left =/= [] andalso Acc#seats.top_right =:= [] ->
      Acc#seats{top_right = RC};
    {LC, _} when LC =/= $\. andalso Acc#seats.top_left =:= [] andalso Acc#seats.top_right =/= [] -> Acc#seats{top_left = LC};
    {_, _} when Acc#seats.top_left =/= [] andalso Acc#seats.top_right =/= [] -> Acc;
    {LC, $\.} when LC =:= Acc#seats.top_left -> Acc;
    {$\., RC} when RC =:= Acc#seats.top_right -> Acc;
    {[], $\.} -> Acc;
    {_, _} -> Acc
  end;
set_seats(Acc, _, _, _, {BottomLeft, BottomRight}, bc) ->
  case {BottomLeft, BottomRight} of
    {$\., $\.} -> Acc;
    {$\., Char} when Acc#seats.bottom_right =:= [] ->
      Acc#seats{bottom_right = Char};
    {Char, $\.} when Acc#seats.bottom_left =:= [] ->
      Acc#seats{bottom_left = Char};
    {LC, RC} when LC =/= $\. andalso RC =/= $\. andalso Acc#seats.bottom_left =:= [] andalso Acc#seats.bottom_right =:= [] ->
      Acc#seats{bottom_left = LC, bottom_right = RC};
    {_, RC} when RC =/= $\. andalso Acc#seats.bottom_left =/= [] andalso Acc#seats.bottom_right =:= [] ->
      Acc#seats{bottom_right = RC};
    {LC, _} when LC =/= $\. andalso Acc#seats.bottom_left =:= [] andalso Acc#seats.bottom_right =/= [] -> Acc#seats{bottom_left = LC};
    {_, _} when Acc#seats.bottom_left =/= [] andalso Acc#seats.bottom_right =/= [] -> Acc;
    {LC, $\.} when LC =:= Acc#seats.bottom_left -> Acc;
    {$\., RC} when RC =:= Acc#seats.bottom_right -> Acc;
    {[], $\.} -> Acc;
    {_, _} -> Acc
  end.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
