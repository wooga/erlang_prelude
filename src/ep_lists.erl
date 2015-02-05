-module(ep_lists).

%% ==================================================================
%% API Function Exports
%% ==================================================================

-export([ find_first/2
        , find_first/3
        , shuffle/1
        , pick/1
        , sample/2
        ]).


%% ==================================================================
%% API Function Definitions
%% ==================================================================

find_first(Pred, List) ->
    P = fun(E) -> not Pred(E) end,
    case lists:dropwhile(P, List) of
        []    -> false;
        [E|_] -> {value, E}
    end.

find_first(Pred, List, Default) ->
    case find_first(Pred, List) of
        {value, E} -> E;
        false      -> Default
    end.


%% shuffle(List1) -> List2
%% Takes a list and randomly shuffles it. Relies on random:uniform
%% inspired by: http://www.trapexit.org/RandomShuffle
shuffle(List) ->
    D = [ {random:uniform(), E} || E <- List ],
    {_, ShuffledList} = lists:unzip(lists:keysort(1, D)),
    ShuffledList.


pick([]) ->
    undefined;
pick(List) ->
    hd(sample(1, List)).


%% return a random sample of N elements from a given List
sample(1, [E]) ->
    [E];
sample(1, List) ->
    lists:sublist(List, random:uniform(length(List)), 1);
sample(N, List) when N >= length(List) ->
    List;
sample(N, List) ->
    lists:sublist(shuffle(List), N).
