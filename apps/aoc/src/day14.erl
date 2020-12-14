-module(day14).

%%%_* Exports ==================================================================

-export([ run/0
        ]).

%%%_* API ======================================================================

run() ->
  Part2Fun = fun(AddrBin, Value, MaskX, Mask0, Mask1, Acc) ->
               AddrMask = binary_to_integer(AddrBin),
               Addr = (AddrMask band Mask0) bor Mask1,
               lists:foldl(fun(X, B) ->
                               maps:put(Addr bor X, Value, B)
                           end, Acc, permutate_bitmask(MaskX, MaskX, [MaskX]))
           end,
  {MapPart1, MapPart2} =
    lists:foldl(
      fun({{_, {MaskX, Mask0, Mask1}}, Memory}, {MapPart1, MapPart2}) ->
          lists:foldl(
            fun({AddrBin, ValueBin}, {MP1, MP2}) ->
                Value = binary_to_integer(ValueBin),
                NewMP1 = maps:put(AddrBin, ((Value band MaskX) bor Mask1), MP1),
                NewMP2 = Part2Fun(AddrBin, Value, MaskX, Mask0, Mask1, MP2),
                {NewMP1, NewMP2}
            end, {MapPart1, MapPart2}, Memory)
      end, {#{}, #{}}, parse(input())),
  {lists:sum(maps:values(MapPart1)), lists:sum(maps:values(MapPart2))}.

%%%_* Internal =================================================================

input() ->
  util:read_file("day14.txt", <<"\n">>, fun(A) -> A end).

parse([<<"mask = ", Mask/binary>> | T]) ->
  MaskX = to_bitmask(Mask, $X),
  Mask0 = to_bitmask(Mask, $0),
  Mask1 = to_bitmask(Mask, $1),
  parse(T, {{1, {MaskX, Mask0, Mask1}}, #{}}).

parse([], {_, Acc}) -> lists:sort(maps:to_list(Acc));
parse([<<"mask = ", Mask/binary>> | T], {{I, _}, Acc}) ->
  MaskX = to_bitmask(Mask, $X),
  Mask0 = to_bitmask(Mask, $0),
  Mask1 = to_bitmask(Mask, $1),
  parse(T, {{I + 1, {MaskX, Mask0, Mask1}}, Acc});
parse([<<"mem", Rest/binary>> | T], {Key, Acc}) ->
  [<<>>, Addr, Value] = re:split(Rest, "\\[([0-9]+)\\] = ([0-9]+)", [{return, binary}, trim]),
  NewAcc = maps:update_with(Key, fun(V) -> V ++ [{Addr, Value}] end, [{Addr, Value}], Acc),
  parse(T, {Key, NewAcc}).

to_bitmask(Bin, Char) ->
  IntBin = << begin case X of Char -> <<"1">>; _ -> <<"0">> end end || <<X>> <= Bin >>,
  binary_to_integer(IntBin, 2).

permutate_bitmask(_BitMask, 0, Acc) -> lists:reverse(Acc);
permutate_bitmask(BitMask, CurrentBitMask, Acc) ->
  NewBitMask = (CurrentBitMask - 1) band BitMask,
  permutate_bitmask(BitMask, NewBitMask, [NewBitMask | Acc]).

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
