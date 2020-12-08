-module(intcode).

%%%_* Exports ==================================================================

-export([ parse/1
        , read_input/1
        , run/5
        , start_acc/0
        ]).

%%%_* Exported =================================================================
read_input(File) ->
  util:read_file(File, <<"\n">>, fun(B) -> binary:split(B, <<" ">>, [trim]) end).

parse(Input) ->
  {_, Map} =
    lists:foldl(
      fun([Op, <<Arith:1/binary, Num/binary>>], {I, Acc}) ->
          {I + 1, maps:put(I, {Op, binary_to_atom(Arith, utf8), binary_to_integer(Num)}, Acc)}
      end, {1, #{}}, Input),
  Map.

run(Maps, Fun, Action, Condition, {{Pos, Acc}, Seen}) when is_function(Fun, 2) andalso
                                                           is_function(Action, 1) andalso
                                                           is_function(Condition, 3) ->
  case maps:get(Pos, hd(Maps), exit) of
    exit -> Acc;
    Value ->
      case Condition(Pos, Value, Seen) of
        true ->
          case Action(Acc) of
            loop -> run(tl(Maps), Fun, Action, Condition, start_acc());
            Acc -> Acc
          end;
        false ->
          run(Maps, Fun, Action, Condition, {Fun(Value, {Pos, Acc}), [{Pos, Value} | Seen]})
      end
  end.

start_acc() -> {{1, 0}, []}.

%%%_* Internal =================================================================

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
