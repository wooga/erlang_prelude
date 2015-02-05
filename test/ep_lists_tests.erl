-module(ep_lists_tests).
-include_lib("eunit/include/eunit.hrl").

-define(IT, ep_lists).


find_first_test() ->
    List = [1, 2, 3, 4],
    Pred = fun (X) -> X < 4 andalso X =/= 1 end,
    False = fun (_) -> false end,
    ?assertEqual({value, 2}, ?IT:find_first(Pred, List)),
    ?assertEqual({value, 3}, ?IT:find_first(Pred, lists:reverse(List))),
    ?assertEqual(default, ?IT:find_first(False, List, default)).


group_by_test() ->
    F = fun erlang:integer_to_list/1,
    L = [1, 2, 3, 3, 2, 1],
    ?assertEqual([{"1", [1, 1]}, {"2", [2, 2]}, {"3", [3, 3]}],
                 lists:sort(?IT:group_by(F, L))).
