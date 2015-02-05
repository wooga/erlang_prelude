-module(ep_http).

-include("conversions.hrl").

-export([ url_decode/1
        , url_encode/1
        , etag/1
        ]).

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


-define(is_alphanum(C), C >= $A, C =< $Z; C >= $a, C =< $z; C >= $0, C =< $9).

-type encodable() :: binary() | atom() | [byte()] | integer().
-spec url_encode(encodable()) -> string().
url_encode(Str) when is_binary(Str) ->
    url_encode(binary_to_list(Str));
url_encode(Str) when is_list(Str) ->
    url_encode(lists:reverse(Str, []), []);
url_encode(Str) when is_integer(Str) ->
    url_encode(integer_to_list(Str));
url_encode(Str) when is_atom(Str) ->
    url_encode(atom_to_list(Str)).

-spec url_encode(list(), list()) -> list().
url_encode([X | T], Acc) when ?is_alphanum(X); X =:= $-; X =:= $_; X =:= $. ->
    url_encode(T, [X | Acc]);
url_encode([32 | T], Acc) ->
    url_encode(T, [$+ | Acc]);
url_encode([X | T], Acc) ->
    NewAcc = [$%, hexchr_encode(X bsr 4), hexchr_encode(X band 16#0f) | Acc],
    url_encode(T, NewAcc);
url_encode([], Acc) ->
    Acc.


etag(Term) ->
    bin_to_hex(crypto:sha(term_to_binary(Term))).


%% ==================================================================
%% Internal functions
%% ==================================================================

-compile({inline, [{hexchr_decode, 1}]}).

hexchr_decode(C) when C >= $a andalso C =< $f ->
    C - $a + 10;
hexchr_decode(C) when C >= $A andalso C =< $F ->
    C - $A + 10;
hexchr_decode(C) when C >= $0 andalso C =< $9 ->
    C - $0;
hexchr_decode(_) ->
    throw({ep_http, bad_input}).

-compile({inline, [{hexchr_encode, 1}]}).

hexchr_encode(N) when N >= 10 andalso N < 16 ->
    N + $A - 10;
hexchr_encode(N) when N >= 0 andalso N < 10 ->
    N + $0.


digit_to_hex(D) when (D >= 0) and (D < 10) ->
    D + 48;
digit_to_hex(D) ->
    D + 87.

bin_to_hex(Bin) ->
    bin_to_hex(?b2l(Bin), []).

bin_to_hex([], Acc) ->
    lists:reverse(Acc);
bin_to_hex([D | Ds], Acc) ->
    bin_to_hex(Ds, [ digit_to_hex(D rem 16)
                   , digit_to_hex(D div 16) | Acc]).
