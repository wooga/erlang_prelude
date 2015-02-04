-module(ep_http).

-include("conversions.hrl").

-export([url_decode/1]).

%%
%% Taken from https://github.com/tim/erlang-percent-encoding/blob/master/src/percent.erl
%% Percent encoding/decoding as defined by the application/x-www-form-urlencoded
%% content type (http://www.w3.org/TR/html4/interact/forms.html#h-17.13.4.1).
%%

url_decode(Str) when is_binary(Str) ->
    ?l2b(url_decode(?b2l(Str), []));
url_decode(A) when is_atom(A) ->
    url_decode(?a2l(A));
url_decode(Str) when is_list(Str) ->
    url_decode(Str, []).

url_decode([$+ | T], Acc) ->
    url_decode(T, [32 | Acc]);
url_decode([$%, A, B | T], Acc) ->
    Char = (hexchr_decode(A) bsl 4) + hexchr_decode(B),
    url_decode(T, [Char | Acc]);
url_decode([X | T], Acc) ->
    url_decode(T, [X | Acc]);
url_decode([], Acc) ->
    lists:reverse(Acc, []).


-compile({inline, [{hexchr_decode, 1}]}).

hexchr_decode(C) when C >= $a andalso C =< $f ->
    C - $a + 10;
hexchr_decode(C) when C >= $A andalso C =< $F ->
    C - $A + 10;
hexchr_decode(C) when C >= $0 andalso C =< $9 ->
    C - $0;
hexchr_decode(_) ->
    throw({ep_http, bad_input}).
