-module(day12).

%%%_* Exports ==================================================================

-export([ part_1/0
        , part_2/0
        ]).

%%%_* API ======================================================================
-spec part_1() -> integer().
part_1() ->
  {{X, Y}, _} = solve(input(), part_1_fun(), {{0, 0}, east}),
  abs(X) + abs(Y).

-spec part_2() -> integer().
part_2() ->
  {_, {ShipX, ShipY}} = solve(input(), part_2_fun(), {{10, -1}, {0, 0}}),
  abs(ShipX) + abs(ShipY).

%%%_* Internal =================================================================

input() ->
  util:read_file("day12.txt", <<"\n">>, fun(A) -> A end).

solve([], _, Acc) -> Acc;
solve([H|T], Fun, Acc) ->
  solve(T, Fun, Fun(H, Acc)).

part_1_fun() ->
  fun(Instruction, {{X, Y}, Direction}) ->
      case Instruction of
        <<"N", Int/binary>> -> {{X, Y - binary_to_integer(Int)}, Direction};
        <<"S", Int/binary>> -> {{X, Y + binary_to_integer(Int)}, Direction};
        <<"W", Int/binary>> -> {{X - binary_to_integer(Int), Y}, Direction};
        <<"E", Int/binary>> -> {{X + binary_to_integer(Int), Y}, Direction};
        <<"F", Int/binary>> -> {forward({X, Y}, binary_to_integer(Int), Direction), Direction};
        <<"L", Int/binary>> -> {{X, Y}, turn_left(Direction, binary_to_integer(Int))};
        <<"R", Int/binary>> -> {{X, Y}, turn_right(Direction, binary_to_integer(Int))}
      end
  end.

part_2_fun() ->
  fun(Instruction, {{WX, WY}, {SX, SY}}) ->
      case Instruction of
        <<"N", Int/binary>> -> {{WX, WY - binary_to_integer(Int)}, {SX, SY}};
        <<"S", Int/binary>> -> {{WX, WY + binary_to_integer(Int)}, {SX, SY}};
        <<"W", Int/binary>> -> {{WX - binary_to_integer(Int), WY}, {SX, SY}};
        <<"E", Int/binary>> -> {{WX + binary_to_integer(Int), WY}, {SX, SY}};
        <<"F", Int/binary>> -> {{WX, WY}, {SX + WX * binary_to_integer(Int), SY + WY * binary_to_integer(Int)}};
        <<"L", Int/binary>> -> {waypoint_rotate_left({WX, WY}, binary_to_integer(Int)), {SX, SY}};
        <<"R", Int/binary>> -> {waypoint_rotate_right({WX, WY}, binary_to_integer(Int)), {SX, SY}}
      end
  end.

forward({X, Y}, Value, north) -> {X, Y - Value};
forward({X, Y}, Value, east)  -> {X + Value, Y};
forward({X, Y}, Value, south) -> {X, Y + Value};
forward({X, Y}, Value, west)  -> {X - Value, Y}.

turn_right(south, 90)  -> west;
turn_right(south, 180) -> north;
turn_right(south, 270) -> east;
turn_right(north, 90)  -> east;
turn_right(north, 180) -> south;
turn_right(north, 270) -> west;
turn_right(west, 90)   -> north;
turn_right(west, 180)  -> east;
turn_right(west, 270)  -> south;
turn_right(east, 90)   -> south;
turn_right(east, 180)  -> west;
turn_right(east, 270)  -> north.

turn_left(south, 90)  -> east;
turn_left(south, 180) -> north;
turn_left(south, 270) -> west;
turn_left(north, 90)  -> west;
turn_left(north, 180) -> south;
turn_left(north, 270) -> east;
turn_left(west, 90)   -> south;
turn_left(west, 180)  -> east;
turn_left(west, 270)  -> north;
turn_left(east, 90)   -> north;
turn_left(east, 180)  -> west;
turn_left(east, 270)  -> south.

waypoint_rotate_left({X, Y}, 90)  -> {Y, -X};
waypoint_rotate_left({X, Y}, 180) -> {-X, -Y};
waypoint_rotate_left({X, Y}, 270) -> {-Y, X}.

waypoint_rotate_right({X, Y}, 90)  -> {-Y, X};
waypoint_rotate_right({X, Y}, 180) -> {-X, -Y};
waypoint_rotate_right({X, Y}, 270) -> {Y, -X}.

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
