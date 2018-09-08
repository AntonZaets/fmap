-module(fmap).

-compile({no_auto_import,[size/1, get/1, put/2]}).

%% API exports
-export([new/2,
         get/2, get/3,
         put/3,
         size/1,
         is_key/2, keys/1, values/1,
         with/2, without/2, with_s/2, without_s/2, take/2,
         merge/2,
         to_list/1,
         to_map/1,
         fold/3,
         map/2,
         update_with/3, update_with/4]).
-export([index_to_key/2]).

-record(fmap, {keys = [], undef = undefined, arr = {}, size = 0}).

%%====================================================================
%% API functions
%%====================================================================
new(Keys, Undefined) ->
    Enumerated = lists:seq(1, length(Keys)),
    {Enumerated,
     #fmap{keys = Keys, undef = Undefined,
           arr = list_to_tuple(lists:duplicate(length(Keys), Undefined)),
           size = 0}}.

get(K, FMap) ->
    do_get(K, FMap, true).

get(K, #fmap{undef = Undefined} = FMap, Default) ->
    case do_get(K, FMap, false) of
        Undefined ->
            Default;
        V ->
            V
    end.

put(K, _V, #fmap{arr = Arr}) when K > erlang:size(Arr) ->
    erlang:throw({badkey, K});
put(K, V, #fmap{arr = Arr, size = S, undef = Undefined} = FMap) ->
    SizeInc =
        case element(K, Arr) of
            Undefined -> 1;
            _ -> 0
        end,
    FMap#fmap{arr = setelement(K, Arr, V), size = S + SizeInc}.

size(#fmap{size = S}) ->
    S.

is_key(K, #fmap{undef = Undefined} = FMap) ->
    case do_get(K, FMap, false) of
        Undefined -> false;
        _ -> true
    end.

keys(#fmap{keys = Keys} = FMap) ->
    do_keys(Keys, FMap, 1, []).

values(#fmap{undef = Undefined, arr = Arr}) ->
    [ El || El <- tuple_to_list(Arr), El =/= Undefined ].

with(Keys, FMap) ->
    with_s(lists:sort(Keys), FMap).

with_s(SortedKeys, FMap) ->
    do_with(SortedKeys, FMap, 1).

without(Keys, FMap) ->
    without_s(lists:sort(Keys), FMap).

without_s(SortedKeys, FMap) ->
    do_without(SortedKeys, FMap, 1).

merge(#fmap{arr = Arr1, undef = Undef1} = FMap1,
      #fmap{arr = Arr2, undef = Undef2} = FMap2)
  when erlang:size(Arr1) =:= erlang:size(Arr2), Undef1 =:= Undef2 ->
    do_merge(FMap1, FMap2, 1);
merge(_, _) ->
    erlang:throw(badarg).

to_list(#fmap{arr = Arr, keys = Keys, undef = Undefined}) ->
    All = lists:zip(Keys, tuple_to_list(Arr)),
    [ El || {_K, V} = El <- All, V =/= Undefined ].

to_map(FMap) ->
    maps:from_list(to_list(FMap)).

fold(Fun, Init, #fmap{keys = Keys} = FMap) ->
    do_fold(Fun, Init, Keys, FMap, 1).

map(Fun, #fmap{keys = Keys} = FMap) ->
    do_map(Fun, Keys, FMap, FMap, 1).

take(Key, #fmap{undef = Undefined} = FMap) ->
    case do_get(Key, FMap, false) of
        Undefined -> error;
        Val -> {Val, without_s([Key], FMap)}
    end.

update_with(Key, Fun, FMap) ->
    V = get(Key, FMap),
    put(Key, Fun(V), FMap).

update_with(Key, Fun, Init, #fmap{undef = Undefined} = FMap) ->
    case do_get(Key, FMap, false) of
        Undefined ->
            put(Key, Init, FMap);
        V ->
            put(Key, Fun(V), FMap)
    end.

index_to_key(Index, #fmap{keys = Keys}) ->
    lists:nth(Index, Keys).

%%====================================================================
%% Internal functions
%%====================================================================
do_keys(_AllKeys, #fmap{arr = Arr}, I, Acc) when I > erlang:size(Arr) ->
    lists:reverse(Acc);
do_keys([K | T], #fmap{arr = Arr, undef = Undefined} = FMap, I, Acc) ->
    case element(I, Arr) of
        Undefined ->
            do_keys(T, FMap, I + 1, Acc);
        _ ->
            do_keys(T, FMap, I + 1, [{K, I} | Acc])
    end.

do_map(_Fun, _AllKeys, #fmap{arr = Arr}, AccMap, I) when I > erlang:size(Arr) ->
    AccMap;
do_map(Fun, [K | T],  #fmap{arr = Arr, undef = Undefined} = SrcMap, AccMap, I) ->
    case element(I, Arr) of
        Undefined ->
            do_map(Fun, T, SrcMap, AccMap, I + 1);
        V ->
            NewAccMap = put(I, Fun({K, I}, V), AccMap),
            do_map(Fun, T, SrcMap, NewAccMap, I + 1)
    end.

do_fold(_Fun, Acc, _AllKeys, #fmap{arr = Arr}, I) when I > erlang:size(Arr)->
    Acc;
do_fold(Fun, Acc, [K | T], #fmap{arr = Arr, undef = Undefined} = FMap, I) ->
    case element(I, Arr) of
        Undefined ->
            do_fold(Fun, Acc, T, FMap, I + 1);
        V ->
            NewAcc = Fun({K, I}, V, Acc),
            do_fold(Fun, NewAcc, T, FMap, I + 1)
    end.

do_merge(#fmap{arr = Arr1} = FMap1, _, I) when I > erlang:size(Arr1) ->
    FMap1;
do_merge(#fmap{arr = Arr1, size = Size} = FMap1,
         #fmap{arr = Arr2, undef = Undefined} = FMap2, I) ->
    case {element(I, Arr1), element(I, Arr2)} of
        {_, Undefined} ->
            do_merge(FMap1, FMap2, I + 1);
        {Undefined, Val2} ->
            NewFMap1 = FMap1#fmap{arr = setelement(I, Arr1, Val2)},
            do_merge(NewFMap1, FMap2, I + 1);
        {_, Val2} ->
            NewFMap1 = FMap1#fmap{arr = setelement(I, Arr1, Val2),
                                  size = Size + 1},
            do_merge(NewFMap1, FMap2, I + 1)
    end.

do_with([], #fmap{arr = Arr} = FMap, I) when I > erlang:size(Arr) ->
    FMap;
do_with([], #fmap{arr = Arr, undef = Undefined, size = Size} = FMap, I) ->
    NewFMap = FMap#fmap{arr = setelement(I, Arr, Undefined),
                        size = Size - 1},
    do_with([], NewFMap, I + 1);
do_with([K | T], FMap, I) when I =:= K ->
    do_with(T, FMap, I + 1);
do_with(Keys, #fmap{arr = Arr, undef = Undefined, size = Size} = FMap, I) ->
    NewFMap = FMap#fmap{arr = setelement(I, Arr, Undefined),
                        size = Size - 1},
    do_with(Keys, NewFMap, I + 1).

do_without(_, #fmap{arr = Arr} = FMap, I) when I > erlang:size(Arr) ->
    FMap;
do_without([], FMap, _I) ->
    FMap;
do_without([K | _] = Keys, FMap, I) when I =/= K ->
    do_without(Keys, FMap, I + 1);
do_without([_ | T], #fmap{arr = Arr, undef = Undefined,
                          size = Size} = FMap, I) ->
    NewFMap = FMap#fmap{arr = setelement(I, Arr, Undefined), size = Size - 1},
    do_without(T, NewFMap, I + 1).

do_get(K, #fmap{arr = Arr}, _Raise)
  when K > erlang:size(Arr); K < -erlang:size(Arr); K == 0 ->
    erlang:throw({badkey, K});
do_get(K, #fmap{arr = Arr, undef = Undefined}, Raise) ->
    Index =
        case K > 0 of
            true ->
                K;
            false ->
                erlang:size(Arr) + K + 1
        end,
    case element(Index, Arr) of
        Undefined when Raise ->
            erlang:throw({badkey, K});
        V ->
            V
    end.


%%====================================================================
%% Tests
%%====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

prepare_data() ->
    [_ | Input] = InputAll = [{a, 1}, {b, 2}, {c, 3}],
    M = maps:from_list(Input),
    {[_ | Ids], FM0} = new([ K || {K, _} <- InputAll ], undefined),
    FM1 = lists:foldl(
        fun({K, {_, V}}, F) ->
            put(K, V, F)
        end,
        FM0,
        lists:zip(Ids, Input)
    ),
    {M, FM1}.

put_get_test_() ->
    {M, FM} = prepare_data(),
    [?_assertEqual(get(2, FM), maps:get(b, M)),
     ?_assertEqual(get(2, FM, null), maps:get(b, M)),
     ?_assertEqual(get(3, FM), maps:get(c, M)),
     ?_assertEqual(get(-1, FM), maps:get(c, M)),
     ?_assertEqual(get(-2, FM), maps:get(b, M)),
     ?_assertEqual(get(1, FM, null), maps:get(a, M, null)),
     ?_assertThrow({badkey, 4}, get(4, FM, null)),
     ?_assertThrow({badkey, 4}, get(4, FM)),
     ?_assertThrow({badkey, -4}, get(-4, FM)),
     ?_assertThrow({badkey, 1}, get(1, FM)),
     ?_assertThrow({badkey, 4}, put(4, val4, FM))].

with_test_() ->
    {_, FM0} = new([a, b, c], undefined),
    FM = put(3, c, put(2, b, (put(1, a, FM0)))),
    [?_assertEqual(get(1, with([1, 2, 3], FM)), a),
     ?_assertEqual(get(2, with([1, 2, 3], FM)), b),
     ?_assertEqual(get(3, with([1, 2, 3], FM)), c),
     ?_assertEqual(get(1, with([1], FM)), a),
     ?_assertEqual(get(2, with([1], FM), undefined), undefined),
     ?_assertEqual(get(3, with([1], FM), undefined), undefined),
     ?_assertEqual(get(1, with([2], FM), undefined), undefined),
     ?_assertEqual(get(2, with([2], FM)), b),
     ?_assertEqual(get(3, with([2], FM), undefined), undefined),
     ?_assertEqual(get(1, with([3], FM), undefined), undefined),
     ?_assertEqual(get(2, with([3], FM), undefined), undefined),
     ?_assertEqual(get(3, with([3], FM)), c),
     ?_assertEqual(FM0, with([], FM)),
     ?_assertEqual(FM0, with([4], FM)),
     ?_assertEqual(1, size(with([2], FM)))].

without_test_() ->
    {_, FM0} = new([a, b, c], undefined),
    FM = put(3, c, put(2, b, (put(1, a, FM0)))),
    [?_assertEqual(FM0, without([1, 2, 3], FM)),
     ?_assertEqual(FM, without([], FM)),
     ?_assertEqual(FM, without([4], FM)),
     ?_assertEqual(get(1, without([2, 3], FM)), a),
     ?_assertEqual(get(2, without([2, 3], FM), undefined), undefined),
     ?_assertEqual(get(3, without([2, 3], FM), undefined), undefined),
     ?_assertEqual(get(1, without([1, 2], FM), undefined), undefined),
     ?_assertEqual(get(2, without([1, 2], FM), undefined), undefined),
     ?_assertEqual(get(3, without([1, 2], FM)), c),
     ?_assertEqual(get(1, without([1, 3], FM), undefined), undefined),
     ?_assertEqual(get(2, without([1, 3], FM)), b),
     ?_assertEqual(get(3, without([1, 3], FM), undefined), undefined),
     ?_assertEqual(1, size(without([1,3], FM)))].

take_test_() ->
    {_, FM0} = new([a, b, c], undefined),
    FM1 = put(1, a, FM0),
    FM2 = put(2, b, FM1),
    FM3 = put(3, c, FM2),
    [?_assertEqual({c, FM2}, take(3, FM3)),
     ?_assertEqual({b, FM1}, take(2, FM2)),
     ?_assertEqual({a, FM0}, take(1, FM1)),
     ?_assertEqual(error, take(1, FM0))].

merge_test_() ->
    {_, FM0} = new([a, b, c], undefined),
    FM1 = put(2, b1, (put(1, a1, FM0))),
    FM2 = put(3, c2, (put(1, a2, FM0))),
    [?_assertEqual(a2, get(1, merge(FM1, FM2))),
     ?_assertEqual(b1, get(2, merge(FM1, FM2))),
     ?_assertEqual(c2, get(3, merge(FM1, FM2))),
     ?_assertEqual(3, size(merge(FM1, FM2)))].

to_list_test() ->
    {M, FM} = prepare_data(),
    ?assertEqual(lists:sort(maps:to_list(M)), lists:sort(to_list(FM))).

to_map_test() ->
    {M, FM} = prepare_data(),
    ?assertEqual(M, to_map(FM)).

is_key_test_() ->
    {_, FM0} = new([a, b], undefined),
    FM = put(1, a, FM0),
    [?_assertEqual(true, is_key(1, FM)),
     ?_assertEqual(false, is_key(2, FM))].

map_test() ->
    {_, FM0} = new([a, b, c], undefined),
    FM = put(3, cval, put(1, aval, FM0)),
    ?assertEqual(#{a => {a, 1, aval}, c => {c, 3, cval}},
                 to_map(map(fun({K, I}, V) -> {K, I, V} end, FM))).

fold_test() ->
    {_, FM0} = new([a, b, c], undefined),
    FM = put(3, cval, put(1, aval, FM0)),
    ?assertEqual([{c, 3, cval}, {a, 1, aval}],
                 fold(fun({K, I}, V, Acc) -> [{K, I, V} | Acc] end, [], FM)).

keys_test() ->
    {_, FM0} = new([a, b, c], undefined),
    FM = put(3, cval, put(1, aval, FM0)),
    ?assertEqual([{a, 1}, {c, 3}], keys(FM)).

values_test() ->
    {_, FM0} = new([a, b, c], undefined),
    FM = put(3, cval, put(1, aval, FM0)),
    ?assertEqual([aval, cval], values(FM)).

update_with_test_() ->
    {_, FM0} = new([a, b], undefined),
    FM = put(1, <<"a">>, FM0),
    [?_assertEqual(#{a => <<"a1">>},
                   to_map(update_with(1, fun(V) -> <<V/binary, "1">> end, FM))),
     ?_assertEqual(#{a => <<"a1">>},
                   to_map(update_with(1, fun(V) -> <<V/binary, "1">> end, some_value, FM))),
     ?_assertThrow({badkey, 2},
                   to_map(update_with(2, fun(_V) -> never_called end, FM))),
     ?_assertEqual(#{a => <<"a">>, b => some_value},
                   to_map(update_with(2, fun(_V) -> never_called end, some_value, FM)))].

index_to_key_test() ->
    {_, FM0} = new([a, b, c], undefined),
    FM = put(3, cval, put(1, aval, FM0)),
    ?assertEqual(c, index_to_key(3, FM)).

-endif.
