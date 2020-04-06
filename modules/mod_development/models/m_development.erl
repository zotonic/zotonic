%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 20019 Marc Worrell
%% @doc Model for supporting the development views.

%% Copyright 2019 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(m_development).

-export([
    m_find_value/3,

    lookup_record/1,
    refresh_records/0,
    load_records/0,
    load_records/1,
    extract_records/1
    ]).

-include("zotonic.hrl").

m_find_value(is_dbtrace, #m{}, Context) ->
    z_development_dbtrace:is_tracing(Context);
m_find_value(list_observers, #m{ value = undefined }, Context) ->
    Observers = z_notifier:get_observers(Context),
    [ {atom_to_binary(Event, utf8), readable(Os)} || {Event, Os} <- Observers ];
m_find_value(record_info, #m{ value = undefined } = M, _Context) ->
    M#m{ value = record_info };
m_find_value(Record, #m{ value = record_info }, _Context) ->
    case to_atom(Record) of
        {ok, Rec} ->
            case lookup_record(Rec) of
                none -> undefined;
                Info -> Info
            end;
        {error, _} ->
            undefined
    end.

to_atom(A) when is_atom(A) -> A;
to_atom(Rec) when is_binary(Rec) ->
    try
        {ok, binary_to_existing_atom(Rec, utf8)}
    catch
        _:_ -> {error, unknown_atom}
    end.

readable(Os) ->
    lists:map(
        fun({Prio, Obs}) ->
            {Prio, iolist_to_binary(readable_1(Obs))}
        end,
        Os).

readable_1(Pid) when is_pid(Pid) ->
    z_html:escape( iolist_to_binary( io_lib:format("~p", [Pid]) ) );
readable_1(F) when is_function(F) ->
    <<"<i>function</i>">>;
readable_1({M, F}) ->
    [
        "<b>", atom_to_binary(M, utf8), "</b>",
        ":", atom_to_binary(F, utf8)
    ];
readable_1({M, F, Args}) ->
    As = [ io_lib:format("~p", [A]) || A <- Args ],
    As1 = z_utils:combine(", ", As),
    [
        "<b>", z_html:escape(atom_to_binary(M, utf8)), "</b>",
        ":", z_html:escape(atom_to_binary(F, utf8)),
        "(", z_html:escape(iolist_to_binary(As1)), ")"
    ].


-spec lookup_record( atom() ) -> {atom(), list()} | none.
lookup_record( Rec ) ->
    case application:get_env(zotonic, cached_record_defs) of
        {ok, List} ->
            proplists:lookup(Rec, List);
        undefined ->
            load_records(),
            lookup_record(Rec)
    end.


-spec refresh_records() -> ok.
refresh_records() ->
    application:unset_env(zotonic, cached_record_defs).

-spec load_records() -> ok.
load_records() ->
    Mods = [ M || {M, _} <- code:all_loaded() ],
    ZMods = lists:filter(fun is_zotonic_module/1, Mods),
    Recs = lists:usort( lists:flatten([ extract_records(M) || M <- ZMods ]) ),
    application:set_env(zotonic, cached_record_defs, Recs).

-spec is_zotonic_module( module() ) -> boolean().
is_zotonic_module(Mod) ->
    Attrs = erlang:get_module_info(Mod, attributes),
    lists:keymember(mod_title, 1, Attrs).

-spec load_records( module() ) -> ok.
load_records(Module) ->
    Current = application:get_env(zotonic, cached_record_defs, []),
    Recs = extract_records(Module),
    New = lists:foldl(
        fun({K, _} = Def, Acc) ->
            lists:keyreplace(K, 1, Acc, Def)
        end,
        Current,
        Recs),
    application:set_env(zotonic, cached_record_defs, New).

-spec extract_records( module() ) -> list( {atom(), list(atom())} ).
extract_records(Module) ->
    case code:which(Module) of
        BeamFile when is_list(BeamFile) ->
            case beam_lib:chunks(BeamFile, [ abstract_code ]) of
                {ok, {_, [ {abstract_code, {_, AbstractCode }} ]} } ->
                    extract_records_abs(AbstractCode);
                _ ->
                    []
            end;

        _Other ->
            []
    end.

%% @doc Extract all record definitions from the abstract code
extract_records_abs( AbstractCode ) ->
   lists:filtermap(
        fun
            ({attribute, _Pos, record, {Name, Fields}}) ->
                {true, {Name, to_field_names(Fields)}};
            (_) ->
                false
        end,
        AbstractCode).

to_field_names(Fields) ->
    [ to_field_name(Field) || Field <- Fields ].

to_field_name({typed_record_field, RecField, _Type}) ->
    to_field_name(RecField);
to_field_name({record_field, _Line, {atom, _, FieldName}}) ->
    {FieldName, <<"undefined">>};
to_field_name({record_field, _Line, {atom, _, FieldName}, InitExpr}) ->
    {FieldName, concrete(InitExpr)}.

concrete({call, _, {remote, _, {atom, _, Module}, {atom, _, Function}}, Args}) ->
    readable_1({Module, Function, Args});
concrete({atom, _, Atom}) ->
    z_html:escape( atom_to_binary(Atom, utf8) );
concrete({record, _, Rec, _}) ->
    z_html:escape( iolist_to_binary([ $#, atom_to_binary(Rec, utf8), "{}" ]) );
concrete(Expr) ->
    z_html:escape(
        z_convert:to_binary(
            io_lib:format("~p", [erl_syntax:concrete(Expr)])
        ) ).



