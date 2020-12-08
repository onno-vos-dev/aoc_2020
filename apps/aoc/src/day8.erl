-module(day8).

%%%_* Exports ==================================================================

-export([ part_1/0
        , part_2/0
        , input/0
        ]).

%%%_* Exported =================================================================

-spec part_1() -> integer().
part_1() ->
  intcode:run([input()],
              instruction_fun(),
              action(return),
              condition(),
              intcode:start_acc()).

-spec part_2() -> integer().
part_2() ->
  intcode:run(swap_instructions(input()),
              instruction_fun(),
              action(loop),
              condition(),
              intcode:start_acc()).

%%%_* Internal =================================================================
input() ->
  intcode:parse(intcode:read_input("day8.txt")).

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

condition() ->
  fun(Pos, Value, Seen) ->
      lists:member({Pos, Value}, Seen)
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
