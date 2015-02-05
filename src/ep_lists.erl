-module(ep_lists).

%% ==================================================================
%% API Function Exports
%% ==================================================================

-export([ find_first/2
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
