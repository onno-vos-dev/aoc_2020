-module(day13).

%%%_* Exports ==================================================================

-export([ part_1/0
        , part_2/0
        ]).

%%%_* API ======================================================================
-spec part_1() -> integer().
part_1() ->
  Input = input(),
  Timestamp = binary_to_integer(hd(Input)),
  BusNumbers = lists:filtermap(fun(<<"x">>) -> false;
                                  (BusNr) -> {true, binary_to_integer(BusNr)}
                               end, binary:split(lists:last(Input), <<",">>, [global, trim])),
  Schedule = schedules(BusNumbers, Timestamp, []),
  {[EarliestTimestamp], BusNr} = hd(lists:sort(Schedule)),
  (EarliestTimestamp - Timestamp) * BusNr.

-spec part_2() -> integer().
part_2() ->
  Input = input(),
  {_, BusNumbersWithOffset} =
    lists:foldl(fun(<<"x">>, {I, Acc}) ->
                    {I + 1, Acc};
                   (BusNrBin, {I, Acc}) ->
                    BusNr = binary_to_integer(BusNrBin),
                    {I + 1, [{BusNr - I, BusNr} | Acc]}
      end, {0, []}, binary:split(lists:last(Input), <<",">>, [global, trim])),
  chinese_remainder(lists:reverse(BusNumbersWithOffset)).

%%%_* Internal =================================================================

input() ->
  util:read_file("day13.txt", <<"\n">>, fun(A) -> A end).

schedules([], _Timestamp, Schedule) -> Schedule;
schedules([BusNr | Tail], Timestamp, Schedule) ->
  EarliestTimestamp = lists:foldl(fun(Num, Acc) ->
                                      case Num rem BusNr =:= 0 of
                                        true -> [Num | Acc];
                                        false -> Acc
                                      end
                                 end, [], lists:seq(Timestamp, Timestamp + BusNr)),
  schedules(Tail, Timestamp, [{lists:sort(EarliestTimestamp), BusNr} | Schedule]).

%%%_* Chinese Remainer Theorem taken from Rosetta Code =========================

egcd(_, 0) -> {1, 0};
egcd(A, B) ->
    {S, T} = egcd(B, A rem B),
    {T, S - (A div B)*T}.

mod_inv(A, B) ->
    {X, Y} = egcd(A, B),
    if
        A*X + B*Y =:= 1 -> X;
        true -> undefined
    end.

mod(A, M) ->
    X = A rem M,
    if
        X < 0 -> X + M;
        true -> X
    end.

calc_inverses([], []) -> [];
calc_inverses([N | Ns], [M | Ms]) ->
    case mod_inv(N, M) of
        undefined -> undefined;
        Inv -> [Inv | calc_inverses(Ns, Ms)]
    end.

chinese_remainder(Congruences) ->
  {Residues, Modulii} = lists:unzip(Congruences),
  ModPI = lists:foldl(fun(A, B) -> A*B end, 1, Modulii),
  CRT_Modulii = [ModPI div M || M <- Modulii],
  case calc_inverses(CRT_Modulii, Modulii) of
    undefined -> undefined;
    Inverses ->
        Solution = lists:sum([A*B || {A,B} <- lists:zip(CRT_Modulii,
                                                        [A*B || {A,B} <- lists:zip(Residues, Inverses)])]),
      mod(Solution, ModPI)
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
