-module(ep_accessors).

%% Activate accessor generation in your model with
%%     -compile({parse_transform, ep_accessors}).
%%
%% Use with include/accessors.hrl
%%
%%     ?ATTR_READER([{prop1, defaultvalue1}, ...]).
%%     ?ATTR_ACCESSOR([{prop1, defaultvalue1}, ...]).
%%     ?ATTR_WRITER([prop1, ...]).
%%
%% to generate getters and/or setters.

-export([parse_transform/2]).

-record(state, { expected = any
               , exports  = []
               , readers  = []
               , writers  = []
               , eof_line = 9999
               , module
               }).


%% ===================================================================
%% Public functions
%% ===================================================================

parse_transform(Forms, Options) ->
    {ParsedForms, S} =
        parse_trans:transform(fun parse_forms/4, #state{}, Forms, Options),

    Fs = [fun (Fms) ->
                  add_exports(Fms, S#state.exports)
          end,
          fun (Fms) ->
                  append_forms(Fms,
                      [ reader_form(S#state.module, P, Def, S#state.eof_line)
                          || {P, Def} <- S#state.readers ]
                  )
          end,
          fun (Fms) ->
                  append_forms(Fms,
                      [ writer_form(S#state.module, P, S#state.eof_line)
                          || P <- S#state.writers ]
                  )
          end,
          fun parse_trans:revert/1
         ],

    NewForms = lists:foldl(fun (F, Fms) -> F(Fms) end, ParsedForms, Fs),
    %% io:format("~n~n~nREWRITTEN~n~p~n", [NewForms]),
    NewForms.


%% ===================================================================
%% Internal functions
%% ===================================================================

parse_forms(eof_marker, {eof, Line}=Form, Ctx, S) ->
    {Form, true, S#state{eof_line = Line,
                         module   = parse_trans:context(module, Ctx)}};
parse_forms(Type, Form, Ctx, S) ->
    case {parse_trans:context(function, Ctx), parse_trans:context(arity, Ctx)} of
        {attr_reader, 0}   -> parse_reader(Type, Form, Ctx, S);
        {attr_writer, 0}   -> parse_writer(Type, Form, Ctx, S);
        {attr_accessor, 0} -> parse_accessor(Type, Form, Ctx, S);
        {_, _}             -> {Form, true, S#state{expected=any}}
    end.


parse_reader(atom, Form, _Ctx, #state{expected=any, exports=Exports}=S) ->
    {Form, true, S#state{expected=clause,
                         exports = [{attr_reader, 0} | Exports]}};
parse_reader(clause, Form, _Ctx, #state{expected=clause}=S) ->
    {Form, true, S#state{expected=list}};
parse_reader(list, Form, _Ctx, #state{expected=list}=S) ->
    {Form, true, S#state{expected=tuple}};
parse_reader(tuple, {tuple, _, [{atom, _, P}, Def]}=Form, _Ctx,
             #state{expected=Expected, exports=Exports, readers=Readers}=S)
  when Expected =:= tuple orelse Expected =:= list ->
    {Form, false, S#state{readers  = [{P, Def} | Readers],
                          exports  = [{P, 1} | Exports],
                          expected = list}};
parse_reader(Type, Form, _Ctx, #state{expected=Expected}) ->
    parse_trans:error("Invalid attr_reader list", Form,
        [{form, Form}, {got, Type}, {expected, Expected}]).


parse_writer(atom, Form, _Ctx, #state{expected=any, exports=Exports}=S) ->
    {Form, true, S#state{expected = clause,
                         exports  = [{attr_writer, 0} | Exports]}};
parse_writer(clause, Form, _Ctx, #state{expected=clause}=S) ->
    {Form, true, S#state{expected=list}};
parse_writer(list, Form, _Ctx, #state{expected=list}=S) ->
    {Form, true, S#state{expected=atom}};
parse_writer(atom, {atom, _, P}=Form, _Ctx,
             #state{expected=atom, exports=Exports, writers=Writers}=S) ->
    {Form, false, S#state{writers  = [P | Writers],
                          exports  = [{P, 2} | Exports],
                          expected = atom}};
parse_writer(Type, Form, _Ctx, #state{expected=Expected}) ->
    parse_trans:error("Invalid attr_writer list", Form,
        [{form, Form}, {got, Type}, {expected, Expected}]).


parse_accessor(atom, Form, _Ctx, #state{expected=any, exports=Exports}=S) ->
    {Form, true, S#state{expected = clause,
                         exports  = [{attr_accessor, 0} | Exports]}};
parse_accessor(clause, Form, _Ctx, #state{expected=clause}=S) ->
    {Form, true, S#state{expected=list}};
parse_accessor(list, Form, _Ctx, #state{expected=list}=S) ->
    {Form, true, S#state{expected=tuple}};
parse_accessor(tuple, {tuple, _, [{atom, _, P}, Def]}=Form, _Ctx,
             #state{expected=Expected, exports=Exports,
                    readers=Readers, writers=Writers}=S)
  when Expected =:= tuple orelse Expected =:= list ->
    {Form, false, S#state{readers  = [{P, Def} | Readers],
                          writers  = [P | Writers],
                          exports  = [{P, 1} | [{P, 2} | Exports]],
                          expected = list}};
parse_accessor(Type, Form, _Ctx, #state{expected=Expected}) ->
    parse_trans:error("Invalid attr_accessor list", Form,
        [{form, Form}, {got, Type}, {expected, Expected}]).


%% @doc AST of a attribute reader including a default value:
%% Name({Mod, _, P}) -> proplists:get_value(Name, P, Default).
%% If Default is a fun it will be evaluated by appying the model to it.
reader_form(Mod, Name, Default, Line) ->
    DefaultClause = case element(1, Default) of
                         % apply model to "default fun"
                         'fun' -> {call,Line,Default,
                                   [{tuple,Line,
                                           [{atom,Line,Mod},
                                            {var,Line,'_T'},
                                            {var,Line,'P'}]}]};

                         % use immediate value directly
                         _     -> Default
                    end,

    {function, Line, Name, 1,
       [{clause,Line,
         [{tuple,Line,[{atom,Line,Mod},{var,Line,'_T'},{var,Line,'P'}]}],
         [],
         [{call,Line,{remote,Line,{atom,Line,proplists},{atom,Line,get_value}},
           [{atom,Line,Name},
            {var,Line,'P'},
            DefaultClause
           ]}]}]
    }.

%% @doc AST of a attribute writer:
%% Name({Mod, T, P}, V) -> {Mod, T, lists:keystore(Name, 1, P, {Name, V})}.
writer_form(Mod, Name, Line) ->
    {function, Line, Name, 2,
       [{clause,Line,
         [{tuple,Line,
           [{atom,Line,Mod},{var,Line,'T'},{var,Line,'P'}]}, {var,Line,'V'}],
         [],
         [{tuple,Line,
           [{atom,Line,Mod},
            {var,Line,'T'},
            {call,Line,{remote,Line,{atom,Line,lists},{atom,Line,keystore}},
             [{atom,Line,Name},
              {integer,Line,1},
              {var,Line,'P'},
              {tuple,Line,[{atom,Line,Name},{var,Line,'V'}]}
             ]}]}]}]
    }.

add_exports(Forms, Exports) ->
    lists:foldl(fun ({M,A}, Acc) ->
                        parse_trans:export_function(M, A, Acc)
                end, Forms, Exports).

append_forms(Forms, AdditionalForms) ->
    parse_trans:do_insert_forms(
        below,
        AdditionalForms,
        Forms,
        parse_trans:initial_context(Forms, [])
    ).
