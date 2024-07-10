%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017-2019 Marc Worrell
%% @doc Model for mod_development

%% Copyright 2017-2019 Marc Worrell
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

-behaviour (zotonic_model).

-export([
    m_get/3,

    lookup_record/1,
    refresh_records/0,
    load_records/0,
    load_records/1,
    extract_records/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"is_dbtrace">> | Rest ], _Msg, Context) ->
    {ok, {z_development_dbtrace:is_tracing(Context), Rest}};
m_get([ Cfg | Rest ], _Msg, Context)
    when Cfg =:= <<"debug_includes">>;
         Cfg =:= <<"debug_blocks">>;
         Cfg =:= <<"enable_api">>;
         Cfg =:= <<"libsep">>;
         Cfg =:= <<"livereload">>;
         Cfg =:= <<"nocache">> ->
    {ok, {m_config:get_boolean(mod_development, Cfg, Context), Rest}};
m_get([ <<"list_observers">> | Rest ], _Msg, Context) ->
    case z_acl:is_allowed(use, mod_development, Context) of
        true ->
            Observers = z_notifier:get_observers(Context),
            List = [ {atom_to_binary(Event, utf8), readable(Os)} || {Event, Os} <- Observers ],
            {ok, {List, Rest}};
        false ->
            {error, eacces}
    end;
m_get([ <<"record_info">>, Record | Rest ], _Msg, _Context) ->
    RecInfo = case to_atom(Record) of
        {ok, Rec} ->
            case lookup_record(Rec) of
                none -> undefined;
                Info -> Info
            end;
        {error, _} ->
            undefined
    end,
    {ok, {RecInfo, Rest}};
m_get([ <<"recompile">> | Rest ], _Msg, Context) ->
    case m_config:get_boolean(mod_development, enable_api, Context) of
        true ->
            ?LOG_NOTICE("Development API triggered recompilation."),
            z_sidejob:start(z, m, [], Context),
            {ok, {<<"started">>, Rest}};
        false ->
            {error, disabled}
    end;
m_get([ <<"flush">> | Rest ], _Msg, Context) ->
    case m_config:get_boolean(mod_development, enable_api, Context) of
        true ->
            ?LOG_NOTICE("Development API triggered cache flush."),
            z:flush(),
            {ok, {<<"flushed">>, Rest}};
        false ->
            {error, disabled}
    end;
m_get([ <<"reindex">> | Rest ], _Msg, Context) ->
    case m_config:get_boolean(mod_development, enable_api, Context) of
        true ->
            ?LOG_NOTICE("Development API triggered module reindex and translation reload."),
            lists:map(
                fun(Ctx) ->
                    z_module_indexer:reindex(Ctx),
                    z_trans_server:load_translations(Ctx)
                end,
                z_sites_manager:get_site_contexts()),
            {ok, {<<"reindexed">>, Rest}};
        false ->
            {error, disabled}
    end;
m_get([ <<"dispatch_info">> | Rest ], _Msg, Context) ->
    case z_acl:is_allowed(use, mod_development, Context) of
        true ->
            {ok, DispatchInfo} = z:dispatch_list(z_context:site(Context)),
            {ok, {DispatchInfo, Rest}};
        false ->
            {error, eacces}
    end;
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.


to_atom(A) when is_atom(A) -> A;
to_atom(Rec) when is_binary(Rec) ->
    try
        {ok, binary_to_existing_atom(Rec, utf8)}
    catch
        _:_ -> {error, unknown_atom}
    end.

readable(Os) ->
    lists:map(
        fun({Prio, Obs, _Pid}) ->
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
    As1 = lists:join(", ", As),
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


