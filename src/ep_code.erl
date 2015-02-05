-module(ep_code).

%% ==================================================================
%% API Function Exports
%% ==================================================================

-export([modules_with_behaviour/1, has_behaviour/2]).

%% ==================================================================
%% API Function Definitions
%% ==================================================================

-spec modules_with_behaviour(module()) -> [module()].
modules_with_behaviour(Behaviour) ->
    lists:filter(fun (Mod) -> has_behaviour(Behaviour, Mod) end,
        [ Mod || {Mod, _} <- code:all_loaded() ]).

-spec has_behaviour(atom(), module()) -> boolean().
has_behaviour(Behaviour, Mod) ->
    lists:any(fun ({behaviour, [B]}) -> B =:= Behaviour;
                  (_)                -> false
              end, Mod:module_info(attributes)).
