%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017-2026 Marc Worrell
%% @doc Model for mod_development
%% @end

%% Copyright 2017-2026 Marc Worrell
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
-moduledoc(#{
    zotonic_keywords => ["reference", "backend_developer", "model", "development_and_debugging", "monitor"]
}).
-moduledoc("
Model for development and diagnostics controls, including tracing flags, observer lists, cache/compile/reindex actions, and dispatch info.

Available Model API Paths
-------------------------

| Method | Path pattern | Description |
| --- | --- | --- |
| `get` | `/is_dbtrace/...` | Return whether database query tracing is currently enabled. |
| `get` | `/+cfg/...` | Resolve the configuration flag `+cfg` and return the matching value via `m_config:get_boolean`. |
| `get` | `/function_tracing_enabled/...` | Return whether function tracing controls are enabled for current user/session. |
| `get` | `/list_observers/...` | Return registered notifier observers for inspection (admin/development access required). |
| `get` | `/record_info/+record/...` | Return runtime record-field metadata for record name `+record`. |
| `get` | `/recompile/...` | Return config flag controlling whether runtime recompile action is enabled. |
| `get` | `/flush/...` | Return config flag controlling whether cache flush action is enabled. |
| `get` | `/reindex/...` | Return config flag controlling whether reindex action is enabled. |
| `get` | `/dispatch_info/...` | Return dispatch rule info/inspection data for development diagnostics. |
| `get` | `/editor` | Detect editors on development sites; requires use of mod_development. Other environments use browser launches without local probes. |
| `post` | `/editor/preference` | Save the machine editor choice; development environment and module ACL required; disabled in containers. |
| `post` | `/editor/open` | Open an indexed template or signed source location; development environment and module ACL required; disabled in containers. |
| `get` | `/show_trace_button/...` | Check if the trace button for debugging templates can be shown. |
| `get` | `/template_trace_parents/...` | Return traced parent relations for a template source location. |
| `get` | `/template_trace_children/...` | Return traced templates included from a template source location. |
| `get` | `/template_trace_filename_menu/...` | Return grouped filename-menu relations for a traced template. |

`/+name` marks a variable path segment. A trailing `/...` means extra path segments are accepted for further lookups.
").

-behaviour (zotonic_model).

-export([
    m_get/3,
    m_post/3,
    dispatch_trace_sources/2,

    lookup_record/1,
    refresh_records/0,
    load_records/0,
    load_records/1,
    extract_records/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"editor">> | Rest ], _Msg, Context) ->
    case z_acl:is_allowed(use, mod_development, Context) of
        true ->
            {ok, #module_index{filepath = File}} = z_module_indexer:find(template, <<"admin_development.tpl">>, Context),
            LocalAllowed = m_site:environment(Context) =:= development,
            Detection = case LocalAllowed of
                true -> z_development_editor:detect(File);
                false -> #{browser_only => true, candidates => [], selected => undefined}
            end,
            %% Executables and arguments are server-owned, never accepted back from the browser.
            Public = fun(C) -> maps:with([id, label, reason, available], C) end,
            Selected = case maps:get(selected, Detection) of undefined -> undefined; C -> Public(C) end,
            Info = Detection#{candidates => [Public(C) || C <- maps:get(candidates, Detection)],
                              selected => Selected, test_file => File,
                              editor_id => z_config:get(development_editor_id, <<"auto">>),
                              local_allowed => LocalAllowed},
            {ok, {Info, Rest}};
        false -> {error, eacces}
    end;
m_get([ <<"is_dbtrace">> | Rest ], _Msg, Context) ->
    {ok, {z_development_dbtrace:is_tracing(Context), Rest}};
m_get([ Cfg | Rest ], _Msg, Context)
    when Cfg =:= <<"debug_includes">>;
         Cfg =:= <<"debug_blocks">>;
         Cfg =:= <<"enable_api">>;
         Cfg =:= <<"libsep">>;
         Cfg =:= <<"trace_button">>;
         Cfg =:= <<"hide_trace_button">>;
         Cfg =:= <<"livereload">>;
         Cfg =:= <<"nocache">> ->
    {ok, {m_config:get_boolean(mod_development, Cfg, Context), Rest}};
m_get([ <<"function_tracing_enabled">> | Rest ], _Msg, Context) ->
    case z_acl:user(Context) of
        ?ACL_ADMIN_USER_ID ->
            {ok, {z_config:get(function_tracing_enabled), Rest}};
        _ ->
            {error, eacces}
    end;
m_get([ <<"show_trace_button">> | Rest ], _Msg, Context) ->
    TraceButton = case z_context:get(zotonic_dispatch_module, Context) of
        mod_development ->
            false;
        _ ->
            case m_config:get_boolean(mod_development, hide_trace_button, Context) of
                true ->
                    false;
                false ->
                    case z_acl:is_allowed(use, mod_development, Context) of
                        true ->
                            case m_site:environment(Context) of
                                development -> true;
                                test -> true;
                                _ -> m_config:get_boolean(mod_development, trace_button, Context)
                            end;
                        false ->
                            false
                    end
            end
    end,
    {ok, {TraceButton, Rest}};
m_get([ <<"list_observers">> | Rest ], _Msg, Context) ->
    case z_acl:is_allowed(use, mod_development, Context) of
        true ->
            Observers = z_notifier:get_observers(Context),
            List = [ {atom_to_binary(Event, utf8), readable(Os, Context)} || {Event, Os} <- Observers ],
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
            Files = dispatch_files(Context),
            Rules = [dispatch_sources(Rule, Files, Context) || Rule <- maps:get(dispatch_list, DispatchInfo)],
            {ok, {DispatchInfo#{dispatch_list => Rules}, Rest}};
        false ->
            {error, eacces}
    end;
m_get([ <<"template_trace_parents">> | Rest ], Msg, Context) ->
    template_trace_relations(parents, Rest, Msg, Context);
m_get([ <<"template_trace_children">> | Rest ], Msg, Context) ->
    template_trace_relations(outgoing, Rest, Msg, Context);
m_get([ <<"template_trace_filename_menu">> | Rest ], Msg, Context) ->
    template_trace_filename_menu(Rest, Msg, Context);
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.

%% @doc Handle authorized development editor launches and machine preference updates.
%% Browser launches accept indexed templates or signed source locations.
%% Shell callers use z:edit/1,2 with the OS user's existing privileges.
-spec m_post(list(binary()), zotonic_model:opt_msg(), z:context()) -> zotonic_model:post_return().
m_post([<<"editor">>, <<"preference">>], #{payload := #{<<"editor">> := Editor}}, Context)
        when is_binary(Editor), byte_size(Editor) > 0, byte_size(Editor) < 128 ->
    case z_acl:is_allowed(use, mod_development, Context)
         andalso m_site:environment(Context) =:= development of
        true ->
            {ok, #module_index{filepath = File}} = z_module_indexer:find(
                template, <<"admin_development.tpl">>, Context),
            z_development_editor:save_preference(Editor, File);
        false -> {error, eacces}
    end;
m_post([<<"editor">>, <<"open">>], #{payload := Payload}, Context) when is_map(Payload) ->
    case z_acl:is_allowed(use, mod_development, Context)
         andalso m_site:environment(Context) =:= development of
        true -> editor_open(Payload, Context);
        false -> {error, eacces}
    end;
m_post(_, _, _) -> {error, unknown_path}.

%% @doc Validate a signed source token or exact indexed template path before launching an editor.
editor_open(#{<<"source">> := Token, <<"editor">> := Editor}, Context)
        when is_binary(Token), byte_size(Token) > 0, byte_size(Token) < 16384,
             is_binary(Editor), byte_size(Editor) > 0, byte_size(Editor) < 128 ->
    Location = try z_crypto:depickle(Token, Context) catch error:_ -> invalid end,
    case Location of
        {development_editor_source, File, Line}
                when is_binary(File), byte_size(File) > 0, byte_size(File) < 4096,
                     is_integer(Line), Line > 0, Line < 10000000 ->
            z_development_editor:open(File, Line, 1, Editor);
        _ -> {error, eacces}
    end;
editor_open(#{<<"source">> := _}, _Context) ->
    {error, badarg};
editor_open(#{<<"file">> := File, <<"editor">> := Editor,
              <<"line">> := Line, <<"column">> := Column}, Context)
    when is_binary(File), byte_size(File) > 0, byte_size(File) < 4096,
         is_binary(Editor), byte_size(Editor) > 0, byte_size(Editor) < 128,
         is_integer(Line), Line > 0, Line < 10000000,
         is_integer(Column), Column > 0, Column < 10000000 ->
    case binary:split(File, <<"/priv/templates/">>, [global]) of
        [_ | [_ | _]] = Parts ->
            Indexed = z_module_indexer:find_all(template, lists:last(Parts), Context),
            case lists:any(fun(#module_index{filepath = P}) -> P =:= File end, Indexed) of
                true -> z_development_editor:open(File, Line, Column, Editor);
                false -> {error, enoent}
            end;
        _ -> {error, enoent}
    end;
editor_open(_, _) -> {error, badarg}.

template_trace_relations(Direction, Rest, Msg, Context) ->
    case z_acl:is_allowed(use, mod_development, Context) of
        true ->
            Payload = case Msg of
                #{ payload := P } when is_map(P) -> P;
                _ -> #{}
            end,
            case maps:get(<<"template">>, Payload, undefined) of
                Template when is_binary(Template) ->
                    {ok, Trace} = mod_development:template_trace_fetch(Context),
                    Graph = maps:get(graph, Trace, #{}),
                    Result = trace_relations(Direction, Template, Payload, Graph),
                    {ok, {Result, Rest}};
                _ ->
                    {error, missing_arg}
            end;
        false ->
            {error, eacces}
    end.

trace_relations(Direction, Template, Payload, Graph) ->
    Nodes = maps:get(nodes, Graph, []),
    Edges = maps:get(edges, Graph, []),
    NodeMap = maps:from_list([ {maps:get(id, N), N} || N <- Nodes ]),
    case find_trace_node(Template, Nodes) of
        undefined ->
            [];
        #{ id := NodeId } ->
            FilterLine = maps:get(<<"line">>, Payload, undefined),
            FilterColumn = maps:get(<<"column">>, Payload, undefined),
            FilterType = maps:get(<<"type">>, Payload, undefined),
            Relations = relations(Direction, NodeId, Template, FilterLine, FilterColumn, FilterType, Edges, NodeMap),
            unique_relations(Relations)
    end.

template_trace_filename_menu(Rest, Msg, Context) ->
    case z_acl:is_allowed(use, mod_development, Context) of
        true ->
            Payload = case Msg of
                #{ payload := P } when is_map(P) -> P;
                _ -> #{}
            end,
            case maps:get(<<"template">>, Payload, undefined) of
                Template when is_binary(Template) ->
                    {ok, Trace} = mod_development:template_trace_fetch(Context),
                    Graph = maps:get(graph, Trace, #{}),
                    {ok, {filename_menu_relations(Template, Graph), Rest}};
                _ ->
                    {error, missing_arg}
            end;
        false ->
            {error, eacces}
    end.

filename_menu_relations(Template, Graph) ->
    Nodes = maps:get(nodes, Graph, []),
    Edges = maps:get(edges, Graph, []),
    NodeMap = maps:from_list([ {maps:get(id, N), N} || N <- Nodes ]),
    case find_trace_node(Template, Nodes) of
        undefined ->
            #{
                extends_overrules => [],
                includes => []
            };
        #{ id := NodeId } ->
            #{
                extends_overrules => unique_relations(
                    lists:filtermap(
                        fun(E) -> filename_menu_relation(extends_overrules, NodeId, E, NodeMap) end,
                        Edges)),
                includes => unique_relations(
                    lists:filtermap(
                        fun(E) -> filename_menu_relation(includes, NodeId, E, NodeMap) end,
                        Edges))
            }
    end.

filename_menu_relation(extends_overrules, NodeId, #{ from := NodeId, type := Type } = Edge, NodeMap)
    when Type =:= extends; Type =:= overrules ->
    edge_relation(to, Edge, NodeMap);
filename_menu_relation(includes, NodeId, #{ to := NodeId, type := include } = Edge, NodeMap) ->
    edge_relation(from, Edge, NodeMap);
filename_menu_relation(_, _NodeId, _Edge, _NodeMap) ->
    false.

relations(parents, NodeId, Template, FilterLine, FilterColumn, FilterType, Edges, NodeMap) ->
    Incoming = lists:filtermap(
        fun(E) ->
            relation(incoming, NodeId, Template, FilterLine, FilterColumn, FilterType, E, NodeMap)
        end,
        Edges),
    SemanticParents = lists:filtermap(
        fun(E) ->
            parent_relation(NodeId, FilterType, E, NodeMap)
        end,
        Edges),
    Incoming ++ SemanticParents;
relations(Direction, NodeId, Template, FilterLine, FilterColumn, FilterType, Edges, NodeMap) ->
    lists:filtermap(
        fun(E) ->
            relation(Direction, NodeId, Template, FilterLine, FilterColumn, FilterType, E, NodeMap)
        end,
        Edges).

find_trace_node(Template, Nodes) ->
    case lists:search(
        fun(#{ filepath := Path }) -> Path =:= Template end,
        Nodes)
    of
        {value, Node} -> Node;
        false -> undefined
    end.

relation(incoming, NodeId, _Template, _FilterLine, _FilterColumn, _FilterType, #{ to := NodeId } = Edge, NodeMap) ->
    edge_relation(from, Edge, NodeMap);
relation(outgoing, NodeId, _Template, FilterLine, FilterColumn, FilterType, #{ from := NodeId } = Edge, NodeMap) ->
    case edge_matches(Edge, FilterLine, FilterColumn, FilterType) of
        true -> edge_relation(to, Edge, NodeMap);
        false -> false
    end;
relation(_, _NodeId, _Template, _FilterLine, _FilterColumn, _FilterType, _Edge, _NodeMap) ->
    false.

parent_relation(NodeId, FilterType, #{ from := NodeId } = Edge, NodeMap) ->
    case maps:get(type, Edge) of
        include ->
            false;
        EdgeType ->
            TypeMatch = case FilterType of
                undefined -> true;
                TypeBin -> atom_to_binary(EdgeType, utf8) =:= TypeBin
            end,
            case TypeMatch of
                true -> edge_relation(to, Edge, NodeMap);
                false -> false
            end
    end;
parent_relation(_NodeId, _FilterType, _Edge, _NodeMap) ->
    false.

edge_matches(Edge, undefined, undefined, undefined) ->
    maps:get(type, Edge, undefined) =/= undefined;
edge_matches(Edge, FilterLine, FilterColumn, FilterType) ->
    EdgeType = maps:get(type, Edge),
    {LineMatch, ColumnMatch} = case EdgeType of
        include ->
            {
                case FilterLine of
                    undefined -> true;
                    Line -> maps:get(line, Edge, 0) =:= z_convert:to_integer(Line)
                end,
                case FilterColumn of
                    undefined -> true;
                    Column -> maps:get(column, Edge, 0) =:= z_convert:to_integer(Column)
                end
            };
        _ ->
            {true, true}
    end,
    TypeMatch = case FilterType of
        undefined -> true;
        TypeBin -> atom_to_binary(EdgeType, utf8) =:= TypeBin
    end,
    LineMatch andalso ColumnMatch andalso TypeMatch.

edge_relation(NodeKey, Edge, NodeMap) ->
    NodeId = maps:get(NodeKey, Edge),
    case maps:get(NodeId, NodeMap, undefined) of
        undefined ->
            false;
        Node ->
            {true, #{
                template => maps:get(filepath, Node),
                module => maps:get(module, Node, <<>>),
                name => maps:get(template, Node, <<>>),
                type => atom_to_binary(maps:get(type, Edge), utf8),
                line => maps:get(line, Edge, 0),
                column => maps:get(column, Edge, 0)
            }}
    end.

unique_relations(Relations) ->
    {Unique, _Seen} = lists:foldl(
        fun(#{ template := Template } = Relation, {Acc, Seen}) ->
            case maps:is_key(Template, Seen) of
                true ->
                    {Acc, Seen};
                false ->
                    {[Relation | Acc], Seen#{ Template => true }}
            end
        end,
        {[], #{}},
        lists:sort(fun relation_sort/2, Relations)),
    lists:reverse(Unique).

relation_sort(A, B) ->
    {maps:get(module, A), maps:get(name, A), maps:get(template, A)} =<
        {maps:get(module, B), maps:get(name, B), maps:get(template, B)}.


to_atom(A) when is_atom(A) -> A;
to_atom(Rec) when is_binary(Rec) ->
    try
        {ok, binary_to_existing_atom(Rec, utf8)}
    catch
        _:_ -> {error, unknown_atom}
    end.

%% @doc Format observer priorities and escaped handler labels with signed editor locations.
readable(Os, Context) ->
    lists:map(
        fun({Prio, Obs, _Pid}) ->
            {Prio, iolist_to_binary(readable_1(Obs)), observer_source(Obs, Context)}
        end,
        Os).

%% @doc Resolve a registered subscriber to a signed source location when source is available.
observer_source({M, F, _Args}, Context) -> observer_source({M, F}, Context);
observer_source({M, F}, Context) when is_atom(M), is_atom(F) ->
    signed_source(z_development_editor:source_location(M, F), Context);
observer_source(Fun, Context) when is_function(Fun) ->
    {module, M} = erlang:fun_info(Fun, module),
    {name, F} = erlang:fun_info(Fun, name),
    observer_source({M, F}, Context);
observer_source(_, _) -> undefined.

%% @doc Add editor locations to the controller-bearing steps of a dispatch trace.
-spec dispatch_trace_sources(Trace, Context) -> Trace1 when
    Trace :: [map()], Context :: z:context(), Trace1 :: [map()].
dispatch_trace_sources(Trace, Context) ->
    case z_acl:is_allowed(use, mod_development, Context) of
        true ->
            Files = dispatch_files(Context),
            lists:map(
                fun(#{args := Args} = Step) ->
                    case maps:from_list(Args) of
                        #{controller := _, controller_options := _} = Rule ->
                            Sources = maps:with([controller_source, dispatch_source],
                                dispatch_sources(Rule, Files, Context)),
                            maps:merge(Step, Sources);
                        _ -> Step
                    end
                end,
                Trace);
        false -> Trace
    end.

%% @doc Index dispatch source paths by module and basename for matching active rule metadata.
dispatch_files(Context) ->
    maps:from_list([
        {{Module, z_convert:to_binary(filename:basename(File))}, File}
        || {Module, ModuleFiles} <- z_module_indexer:dispatch(Context), File <- ModuleFiles
    ]).

%% @doc Attach signed controller and dispatch-file locations to an active dispatch rule.
%% Source paths come from the site's active dispatch rules, never browser input.
dispatch_sources(#{controller := Controller, controller_options := Options} = Rule, Files, Context) ->
    Key = {proplists:get_value(zotonic_dispatch_module, Options),
           proplists:get_value(zotonic_dispatch_file, Options)},
    DispatchSource = case maps:get(Key, Files, undefined) of
        undefined -> undefined;
        File -> signed_source({ok, File, 1}, Context)
    end,
    Rule#{
        controller_source => signed_source(z_development_editor:source_location(Controller), Context),
        dispatch_source => DispatchSource
    }.

%% @doc Sign an existing source file and line for browser editor requests; omit unavailable files.
signed_source({ok, File, Line}, Context) ->
    case filelib:is_regular(File) of
        true ->
            FileB = z_convert:to_binary(filename:absname(File)),
            #{file => FileB, line => Line,
              token => z_crypto:pickle({development_editor_source, FileB, Line}, Context)};
        false -> undefined
    end;
signed_source({error, _}, _Context) -> undefined.

readable_1(Pid) when is_pid(Pid) ->
    z_html:escape( iolist_to_binary( io_lib:format("~p", [Pid]) ) );
readable_1(F) when is_function(F) ->
    <<"<i>function</i>">>;
readable_1({M, F}) ->
    [
        "<b>", z_html:escape(atom_to_binary(M, utf8)), "</b>",
        ":", z_html:escape(atom_to_binary(F, utf8))
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
