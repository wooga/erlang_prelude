-module(ep_code_tests).

-include_lib("eunit/include/eunit.hrl").

-define(IT, ep_code).

-behaviour(ep_deploy). % just to test has_behaviour/2

has_behaviour_test() ->
    ?assertEqual(true, ?IT:has_behaviour(ep_deploy, ?MODULE)),
    ?assertEqual(false, ?IT:has_behaviour(not_existing, ?MODULE)),
    ?assertEqual(false, ?IT:has_behaviour(ep_deploy, ?IT)).
