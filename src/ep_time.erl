-module(ep_time).

%% @doc Gregorian seconds at the UNIX epoch, calculated by
%% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})
-define(UNIX_EPOCH, 62167219200).


%% ==================================================================
%% API Function Exports
%% ==================================================================

-export([ timestamp/0
        , timestamp/1
        ]).


%% ==================================================================
%% API Function Definitions
%% ==================================================================

%% @doc get the current "UNIX" timestamp (seconds since the epoch)
timestamp() ->
    timestamp(os:timestamp()).

%% @doc get the "UNIX" timestamp (seconds since the epoch) for a given Erlang
%% Time or DateTime tuple.
timestamp({MegaSeconds, Seconds, _}) ->
    MegaSeconds * 1000000 + Seconds;
timestamp({{_Year, _Month, _Day}, {_Hours, _Minutes, _Seconds}} = DateTime) ->
     calendar:datetime_to_gregorian_seconds(DateTime) - ?UNIX_EPOCH.
