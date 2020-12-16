%% @doc TODO: Not my proudest code... It's a mess and needs cleanup!
-module(day16).

%%%_* Exports ==================================================================

-export([ part_1/0
        , part_2/0
        ]).

%%%_* API ======================================================================
-spec part_1() -> integer().
part_1() ->
  {Rules0, _MyTicket, NearbyTickets} = parse(input()),
  Rules = rules_to_sets(Rules0),
  #{invalid := Invalid} = split_valid_and_invalid(NearbyTickets, Rules),
  20975 = lists:sum(lists:flatten(Invalid)).

-spec part_2() -> integer().
part_2() ->
  {Rules0, MyTicket, NearbyTickets} = parse(input()),
  Rules = rules_to_sets(Rules0),
  #{valid := Valid} = split_valid_and_invalid(NearbyTickets, Rules),
  DeparturePositions = departure_positions(Valid, Rules),
  910339449193 = lists:foldl(fun(Pos, Acc) ->
                                 lists:nth(Pos, MyTicket) * Acc
                             end, lists:nth(hd(DeparturePositions), MyTicket), tl(DeparturePositions)).

%%%_* Parsers ==================================================================

input() ->
  util:read_file("day16.txt", <<"\n\n">>, fun(A) -> A end).

parse([Rules, MyTicket, NearbyTickets]) ->
  {parse_rules(Rules), parse_my_ticket(MyTicket), parse_nearby_tickets(NearbyTickets)}.

parse_rules(Rules) ->
  Regex = "(.*): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)",
  Opts = [{return, binary}, trim],
  B2I = fun erlang:binary_to_integer/1,
  lists:map(
    fun(Op) ->
        [<<>>, Type, LowerLeft, UpperLeft, LowerRight, UpperRight] = re:split(Op, Regex, Opts),
        {Type, {B2I(LowerLeft), B2I(UpperLeft)}, {B2I(LowerRight), B2I(UpperRight)}}
    end, binary:split(Rules, <<"\n">>, [global, trim])).

parse_my_ticket(MyTicket) ->
  [_|MyTickets] = binary:split(MyTicket, [<<",">>, <<"\n">>], [global, trim]),
  lists:map(fun erlang:binary_to_integer/1, MyTickets).

parse_nearby_tickets(NearbyTickets) ->
  [_|Tickets] = binary:split(NearbyTickets, [<<"\n">>], [global, trim]),
  Tickets.

%%%_* Internal =================================================================

rules_to_sets(Rules0) ->
  lists:map(fun({Type, {LowerLeft, UpperLeft}, {LowerRight, UpperRight}}) ->
                Left = lists:seq(LowerLeft, UpperLeft),
                Right = lists:seq(LowerRight, UpperRight),
                {Type, sets:from_list(Left ++ Right)}
            end, Rules0).

%% It's a bit strange to put the whole ticket in #{valid := Valid} while I only put
%% the invalid ticket num in #{invalid := Invalid} but we don't need more for part1
%% while in part two we need the whole ticket so... It'll do for now
split_valid_and_invalid(Tickets, Rules) ->
  lists:foldl(
    fun(Ticket, Acc) ->
        case maps:get(invalid, valid_ticket(Ticket, Rules), []) of
          [] ->
            maps:update_with(valid, fun(V) -> [Ticket | V] end, [Ticket], Acc);
          InvalidNums ->
            maps:update_with(invalid, fun(V) -> [InvalidNums | V] end, [InvalidNums], Acc)
        end
    end, #{}, Tickets).

valid_ticket(Ticket, Rules) ->
  TicketNumbers = ticket_to_numbers(Ticket),
  lists:foldl(fun(Num, Acc) ->
                  case lists:any(fun({_, Set}) -> sets:is_element(Num, Set) end, Rules) of
                    true ->
                      maps:update_with(valid, fun(V) -> [Num | V] end, [Num], Acc);
                    false ->
                      maps:update_with(invalid, fun(V) -> [Num | V] end, [Num], Acc)
                  end
              end, #{}, TicketNumbers).

ticket_to_numbers(Ticket) ->
  lists:map(fun erlang:binary_to_integer/1, binary:split(Ticket, <<",">>, [global, trim])).

departure_positions(Valid, Rules) ->
  Positions = map_positions_to_rules(Valid, Rules),
  lists:flatten(lists:filtermap(fun({<<"departure", _/binary>>, Pos}) -> {true, Pos};
                                   (_) -> false
                                end, Positions)).

map_positions_to_rules(Valid, Rules) ->
  Positions = lists:foldl(
                fun(Ticket, Map) ->
                    find_and_eliminate_non_matching_positions(Ticket, Rules, Map)
                end, start_positions(Rules), Valid),
  find_final_positions(maps:to_list(Positions), maps:to_list(Positions), []).

find_and_eliminate_non_matching_positions(Ticket, Rules, Map) ->
  {_, NewMap} = lists:foldl(
                  fun(Num, {Pos, Acc}) ->
                      NewAcc = lists:foldl(
                                 fun({Type, Set}, A) ->
                                     case sets:is_element(Num, Set) of
                                       true ->
                                         A;
                                       false ->
                                         maps:update_with(Type, fun(V) -> V -- [Pos] end, A)
                                     end
                                 end, Acc, Rules),
                      {Pos + 1, NewAcc}
                  end, {1, Map}, ticket_to_numbers(Ticket)),
  NewMap.

find_final_positions([], [], Acc) -> Acc;
find_final_positions([{Type, Positions}|_], L, Acc) when length(Positions) =:= 1 ->
  NewL = lists:foldl(fun({T, P}, OldList) ->
                         lists:keyreplace(T, 1, OldList, {T, P -- Positions})
                     end, L -- [{Type, Positions}], L -- [{Type, Positions}]),
  find_final_positions(NewL, NewL, [{Type, Positions} | Acc]);
find_final_positions([_|T], L, Acc) ->
  find_final_positions(T, L, Acc).

start_positions(Rules) ->
  maps:from_list(
    lists:map(fun({Type, _}) ->
                  {Type, lists:seq(1,20)}
              end, Rules)).

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
