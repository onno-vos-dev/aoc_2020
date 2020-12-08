-module(day8).

%%%_* Exports ==================================================================

-export([ part_1/0
        , part_2/0
        , input/0
        ]).

%%%_* Exported =================================================================

% 221, 223, 224 -> too low
-spec part_1() -> integer().
part_1() ->
  solve([input()], instruction_fun(), action(return), start_acc()).

-spec part_2() -> integer().
part_2() ->
  solve(swap_instructions(input()), instruction_fun(), action(loop), start_acc()).

%%%_* Internal =================================================================
input() ->
  parse(util:read_file("day8.txt", <<"\n">>, fun(B) -> binary:split(B, <<" ">>, [trim]) end)).

solve(Maps, Fun, Action, {{Pos, Acc}, Seen}) ->
  case maps:get(Pos, hd(Maps), exit) of
    exit -> Acc;
    Value ->
      case lists:member({Pos, Value}, Seen) of
        true ->
          case Action(Acc) of
            loop -> solve(tl(Maps), Fun, Action, start_acc());
            Acc -> Acc
          end;
        false ->
          solve(Maps, Fun, Action, {Fun(Value, {Pos, Acc}), [{Pos, Value} | Seen]})
      end
  end.

parse(Input) ->
  {_, Map} =
    lists:foldl(
      fun([Op, <<Arith:1/binary, Num/binary>>], {I, Acc}) ->
          {I + 1, maps:put(I, {Op, binary_to_atom(Arith, utf8), binary_to_integer(Num)}, Acc)}
      end, {1, #{}}, Input),
  Map.

instruction_fun() ->
  fun({<<"nop">>, _Instr, _}, {Pos, Acc}) -> {Pos + 1, Acc};
     ({<<"acc">>, Instr, Num}, {Pos, Acc}) -> {Pos + 1, erlang:apply(erlang, Instr, [Acc, Num])};
     ({<<"jmp">>, Instr, Num}, {Pos, Acc}) -> {erlang:apply(erlang, Instr, [Pos, Num]), Acc}
  end.

swap_instructions(Input) ->
  Swaps = maps:fold(fun(K, {Op, Instr, Num}, Acc) when Op =:= <<"nop">> orelse Op =:= <<"jmp">> ->
                        maps:put(K, {swap(Op), Instr, Num}, Acc);
                       (_, _, Acc) ->
                        Acc
                    end, #{}, Input),
  maps:values(
    maps:map(fun(K, V) ->
                 maps:update(K, V, Input)
             end, Swaps)).

swap(<<"nop">>) -> <<"jmp">>;
swap(<<"jmp">>) -> <<"nop">>.

action(loop) -> fun(_) -> loop end;
action(return) -> fun(Acc) -> Acc end.

start_acc() -> {{1, 0}, []}.

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
