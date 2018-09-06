-module(fmap_id).

-ifdef(FMAP_ID_NATIVE).
-compile([native, {hipe, [o3]}]).
-endif.

%% API exports
-export([new/2, get/2, get/3, put/3]).%, with/2, without/2, merge/2, keys/2, values/1]).

-record(fmap_id, {keys = [], undef = undefined, arr = {}}).

%%====================================================================
%% API functions
%%====================================================================
new(Keys, Undefined) ->
    Enumerated = lists:seq(1, length(Keys)),
    {Enumerated,
     #fmap_id{keys = Keys, undef = Undefined,
              arr = list_to_tuple(lists:duplicate(length(Keys), Undefined))}}.

get(K, FMap) ->
    do_get(K, FMap, true).

get(K, #fmap_id{undef = Undefined} = FMap, Default) ->
    case do_get(K, FMap, false) of
        Undefined ->
            Default;
        V ->
            V
    end.

put(K, _V, #fmap_id{arr = Arr}) when K > size(Arr) ->
    erlang:throw({badkey, K});
put(K, V, #fmap_id{arr = Arr} = FMap) ->
    FMap#fmap_id{arr = setelement(K, Arr, V)}.


%%====================================================================
%% Internal functions
%%====================================================================

do_get(K, #fmap_id{arr = Arr}, _Raise)
  when K > size(Arr); K < -size(Arr); K == 0 ->
    erlang:throw({badkey, K});
do_get(K, #fmap_id{arr = Arr, undef = Undefined}, Raise) ->
    Index =
        case K > 0 of
            true ->
                K;
            false ->
                size(Arr) + K + 1
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
     ?_assertEqual(get(3, FM), maps:get(c, M)),
     ?_assertEqual(get(-1, FM), maps:get(c, M)),
     ?_assertEqual(get(-2, FM), maps:get(b, M)),
     ?_assertEqual(get(1, FM, null), maps:get(a, M, null)),
     ?_assertThrow({badkey, 4}, get(4, FM, null)),
     ?_assertThrow({badkey, 4}, get(4, FM)),
     ?_assertThrow({badkey, -4}, get(-4, FM)),
     ?_assertThrow({badkey, 1}, get(1, FM))].

-endif.
