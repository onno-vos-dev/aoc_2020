-module(day4).

%%%_* Exports ==================================================================

-export([ part_1/0
        , part_2/0
        ]).

%%%_* Exported =================================================================

-spec part_1() -> integer().
part_1() ->
  solve(input(), [ are_keys_present_f() ], 0).

-spec part_2() -> integer().
part_2() ->
  solve(input(), [ are_keys_present_f(), are_keys_valid_f() ], 0).

%%%_* Internal =================================================================
input() ->
  F = fun(B) ->
          binary:split(binary:replace(B, <<"\n">>, <<" ">>, [global]), <<" ">>, [trim, global])
      end,
  util:read_file("day4.txt", <<"\n\n">>, F).

solve([], _Funs, Acc) -> Acc;
solve([H | T], Funs, Acc) ->
  Split = lists:map(fun(String) ->
                        binary:split(String, <<":">>)
                    end, H),
  IsValid = lists:all(fun(F) ->
                          F(Split)
                      end, Funs),
  NewAcc = case IsValid of
             true -> Acc + 1;
             false -> Acc
           end,
  solve(T, Funs, NewAcc).

are_keys_present_f() ->
  fun(Strings) ->
      Keys = lists:map(fun([Key, _Value]) -> Key end, Strings),
      case required_keys() -- Keys of
        [] -> true;
        [<<"cid">>] -> true;
        _ -> false
      end
  end.

are_keys_valid_f() ->
  fun(Strings) ->
      Res = lists:map(fun([Key, Value]) ->
                          is_valid_value(Key, Value)
                      end, Strings),
      lists:all(fun(true) -> true; (false) -> false end, Res)
  end.

required_keys() ->
  [<<"byr">>, <<"iyr">>, <<"eyr">>, <<"hgt">>, <<"hcl">>, <<"ecl">>, <<"pid">>].

is_valid_value(<<"byr">>, Value) when Value >= <<"1920">> andalso Value =< <<"2002">> -> true;
is_valid_value(<<"iyr">>, Value) when Value >= <<"2010">> andalso Value =< <<"2020">> -> true;
is_valid_value(<<"eyr">>, Value) when Value >= <<"2020">> andalso Value =< <<"2030">> -> true;
is_valid_value(<<"hgt">>, <<Value:3/binary, "cm">>) when Value >= <<"150">> andalso
                                                         Value =< <<"193">> -> true;
is_valid_value(<<"hgt">>, <<Value:2/binary, "in">>) when Value >= <<"59">> andalso
                                                         Value =< <<"75">> -> true;
is_valid_value(<<"ecl">>, Value) -> lists:member(Value, valid_eye_colors());
is_valid_value(<<"hcl">>, <<"#", Value:6/binary>>) ->
  case re:run(Value, "([0-9a-f]*)") of
    {match, _} -> true;
    nomatch -> false
  end;
is_valid_value(<<"pid">>, <<Value:9/binary>>) ->
  case re:run(Value, "([0-9]*)") of
    {match, _} -> true;
    nomatch -> false
  end;
is_valid_value(<<"cid">>, _) -> true;
is_valid_value(_, _) -> false.

valid_eye_colors() ->
  [<<"amb">>, <<"blu">>, <<"brn">>, <<"gry">>, <<"grn">>, <<"hzl">>, <<"oth">>].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
