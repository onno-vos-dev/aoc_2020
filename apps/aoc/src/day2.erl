-module(day2).

%%%_* Exports ==================================================================

-export([ part_1/0
        , part_2/0

         ]).

%%%_* Exported =================================================================

-spec part_1() -> integer().
part_1() ->
  length(lists:filter(fun(X) -> X end, solve(input(), part_1_fun(), []))).

-spec part_2() -> integer().
part_2() ->
  length(lists:filter(fun(X) -> X end, solve(input(), part_2_fun(), []))).

%%%_* Internal =================================================================
input() ->
  util:read_file("day2.txt", <<"\n">>, fun erlang:binary_to_list/1).

solve([], _Fun, Acc) -> Acc;
solve([H | T], Fun, Acc) when is_function(Fun, 1) ->
  solve(T, Fun, [ Fun(H) | Acc]).

split(H) ->
  [X, Y, Password] = string:tokens(H, " "),
  Nums = [ list_to_integer(I) || I <- string:tokens(X, "-") ],
  Char = string:strip(Y, right, $:),
  {Nums, Char, Password}.

part_1_fun() ->
  fun(H) ->
      {[Min, Max], Char, Password} = split(H),
      X = length(lists:flatten(string:tokens(Password, Char))),
      Y = length(Password),
      (Y - X) >= Min andalso (Y - X) =< Max
  end.

part_2_fun() ->
  fun(H) ->
      {[Min, Max], Char, Password} = split(H),
      MinChar = string:substr(Password, Min, 1),
      MaxChar = string:substr(Password, Max, 1),
      (MinChar =:= Char orelse MaxChar =:= Char) andalso not (MinChar =:= MaxChar)
  end.

%%%_* EUnit ====================================================================
-ifdef(EUNIT).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").

day1a_example_test() ->
  ?assertEqual(2,
               length(lists:filter(fun(X) -> X end,
                                   solve(["1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc"],
                                         part_1_fun(),
                                         [])))).

day1b_example_test() ->
  ?assertEqual(1,
               length(lists:filter(fun(X) -> X end,
                                   solve(["1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc"],
                                         part_2_fun(),
                                         [])))).

-endif.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
