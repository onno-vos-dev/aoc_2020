-module(day15).

%%%_* Exports ==================================================================

-export([ part_1/1
        , part_2/1
        , benchmark/2
        ]).

-type store_type() :: ets | map | process_dictionary | counters | list | dict.
-type store() :: any().

%%%_* API ======================================================================
benchmark(Part, StoreTypes) when is_atom(Part) andalso is_list(StoreTypes) ->
  lists:foreach(fun(StoreType) ->
                    {Time, _} = timer:tc(fun() -> ?MODULE:Part(StoreType) end),
                    io:format("Time for ~p -> ~p us~n", [StoreType, Time])
                end, StoreTypes).

-spec part_1(store_type()) -> integer().
part_1(StoreType) ->
  Input = [1,2,16,19,18,0],
  Limit = 2021,
  {_, Store} = lists:foldl(fun(Num, {I, Store}) ->
                               {I + 1, store(Num, I, Store, StoreType)}
                           end, {1, new_store(StoreType, ?FUNCTION_NAME, Limit)}, Input),
  {NewStore, Answer} = solve(Limit, length(Input) + 1, Store, StoreType, lists:last(Input)),
  destroy(NewStore, StoreType),
  536 = Answer.

-spec part_2(store_type()) -> integer().
part_2(StoreType) ->
  Input = [1,2,16,19,18,0],
  Limit = 30000001,
  {_, Store} = lists:foldl(fun(Num, {I, Store}) ->
                          {I + 1, store(Num, I, Store, StoreType)}
                      end, {1, new_store(StoreType, ?FUNCTION_NAME, Limit)}, Input),
  {NewStore, Answer} = solve(Limit, length(Input) + 1, Store, StoreType, lists:last(Input)),
  destroy(NewStore, StoreType),
  24065124 = Answer.

%%%_* Internal =================================================================
solve(Nth, I, Store, _, Acc) when Nth =:= I -> {Store, Acc};
solve(Nth, I, Store, StoreType, PrevNum) ->
  NextNum = case lookup(PrevNum, Store, StoreType) of
              [] -> 0;
              Last -> I - Last - 1
            end,
  solve(Nth, I + 1,
        store(PrevNum, I - 1, Store, StoreType),
        StoreType,
        NextNum).

%%%_* Stores ===================================================================
-spec new_store(store_type(), atom(), integer()) -> store().
new_store(list, _, _) -> [];
new_store(map, _, _) -> #{};
new_store(process_dictionary, _, _) -> ignore;
new_store(counters, _, Size) -> counters:new(Size, []);
new_store(ets, Name, _) -> ets:new(Name, [set]);
new_store(dict, _, _) -> dict:new().

-spec store(pos_integer(), pos_integer(), store(), store_type()) -> store().
store(Num, Turns, Store, list) ->
  lists:keystore(Num, 1, Store, {Num, Turns});
store(Num, Turns, Store, map) ->
  maps:put(Num, Turns, Store);
store(Num, Turns, Store, ets) ->
  true = ets:insert(Store, {Num, Turns}),
  Store;
store(Num, Turns, _Store, process_dictionary) ->
  put(Num, Turns);
store(Num, Turns, Store, counters) ->
  ok = counters:put(Store, Num + 1, Turns),
  Store;
store(Num, Turns, Store, dict) ->
  dict:store(Num, Turns, Store).

-spec lookup(pos_integer(), store(), store_type()) -> pos_integer().
lookup(Num, Store, list) ->
  case lists:keyfind(Num, 1, Store) of
    false -> [];
    {_Key, Value} -> Value
  end;
lookup(Num, Store, map) ->
  maps:get(Num, Store, []);
lookup(Num, Store, ets) ->
  case ets:lookup(Store, Num) of
    [] -> [];
    [{_Key, Value}] -> Value
  end;
lookup(Num, _Store, process_dictionary) ->
  case get(Num) of
    undefined -> [];
    Value -> Value
  end;
lookup(Num, Store, counters) ->
  case counters:get(Store, Num + 1) of
    0 -> [];
    Value -> Value
  end;
lookup(Num, Store, dict) ->
  case dict:find(Num, Store) of
    error -> [];
    {ok, Value} -> Value
  end.

-spec destroy(store(), store_type()) -> ok.
destroy(_Store, list) -> ok;
destroy(_Store, map) -> ok;
destroy(_Store, process_dictionary) ->
  erase(),
  ok;
destroy(Store, ets) ->
  ets:delete(Store),
  ok;
destroy(_Store, counters) -> ok;
destroy(_Store, dict) -> ok.

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
