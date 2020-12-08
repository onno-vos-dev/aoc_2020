-module(day7).

%%%_* Exports ==================================================================

-export([ part_1/0
        , part_2/0
        ]).

%%%_* Exported =================================================================

%% 139
-spec part_1() -> integer().
part_1() ->
  M = maps:from_list(input()),
  sets:size(find_bags(M, ["shiny gold"], sets:from_list(["shiny gold"]))) - 1.

%% 58175
-spec part_2() -> integer().
part_2() ->
  M = maps:from_list(input()),
  required_bags(M, "shiny gold").

%%%_* Internal =================================================================
input() ->
  util:read_file("day7.txt", <<"\n">>, fun(B) -> parse_string(binary_to_list(B)) end).

parse_string(String) ->
  [BagType, Rest] = string:split(String, "contain"),
  case Rest of
    " no other bags." ->
      {string:trim(BagType, trailing, "bags"), []};
    _ ->
      SplitRest = [ string:trim(X, both) || X <- string:tokens(Rest, ",") ],
      {string:trim(BagType, trailing, "bags "), split_right(SplitRest)}
  end.

split_right(SplitRest) ->
  lists:map(fun([_, IntStr, Bag]) ->
                {list_to_integer(IntStr), string:trim(Bag, trailing, " bags.")}
            end, [ re:split(Y, "([0-9]+) (.*)", [trim, {return, list}]) || Y <- SplitRest ]).

find_bags(_Map, [], Acc) -> Acc;
find_bags(Map, [Key | T], Acc) ->
  Filtered = maps:filter(fun(_K, V) -> lists:keyfind(Key, 2, V) =/= false end, Map),
  io:format("Filtered: ~p~n~n", [Filtered]),
  find_bags(Map, maps:keys(Filtered) ++ T, sets:union(sets:from_list(maps:keys(Filtered)), Acc)).

required_bags(Map, Key) ->
  lists:foldl(fun({NumBags, Bag}, Acc) ->
                  NumBags * (1 + required_bags(Map, Bag)) + Acc
              end, 0, maps:get(Key, Map, [])).

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
