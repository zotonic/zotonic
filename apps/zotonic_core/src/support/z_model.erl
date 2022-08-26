%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2018-2021 Marc Worrell
%%
%% @doc Call models, direct or via MQTT

%% Copyright 2018-2021 Marc Worrell
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

-module(z_model).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    payload_msg/1,

    template_get/4,

    call/5,
    publish/5,
    callback/5,

    get_module/2,
    get_arg/3
]).

-include_lib("../../include/zotonic.hrl").

-type model_name() :: binary() | atom().
-type verb() :: get | post | delete.
-type model_callback() :: m_get | m_post | m_delete.
-type path_element() :: atom() | binary() | term().
-type path() :: list( path_element() ).
-type opt_message() :: mqtt_packet_map:mqtt_message() | undefined.

-export_type([
    model_name/0,
    verb/0,
    model_callback/0,
    path_element/0,
    path/0,
    opt_message/0
]).

-define(MQTT_CALL_TIMEOUT, 60000).

-spec payload_msg( Payload :: term() ) -> opt_message().
payload_msg(None) when None =:= null; None =:= undefined ->
    #{
        type => publish,
        topic => [],
        properties => #{},
        payload => undefined
    };
payload_msg(Payload) ->
    #{
        type => publish,
        topic => [],
        properties => #{},
        payload => Payload
    }.

%% @doc Called by the compiled templates for a model lookup. Any map in the path is taken as the
%% model payload, and passed in the msg part of the model m_get call.
-spec template_get( model_name(), path(), term(), z:context() ) -> {ok, {term(), path()}} | {error, term()}.
template_get( Model, Path, Payload, Context ) ->
    Msg = payload_msg(Payload),
    case get_module(Model, Context) of
        {ok, Mod} ->
            model_call(Mod, m_get, Path, Msg, Context);
        {error, _} ->
            case publish(Model, get, Path, Msg, Context) of
                {ok, RespPayload} ->
                    {ok, {RespPayload, []}};
                {error, _} = Error ->
                    Error
            end
    end.


-spec call( model_name(), verb(), path(), opt_message(), z:context() ) -> {ok, term()} | {error, term()}.
call(Model, Verb, Path, Msg, Context) ->
    case get_module(Model, Context) of
        {ok, Mod} ->
            maybe_resolve(Verb, model_call(Mod, map_verb(Verb), Path, Msg, Context), Context);
        {error, _} ->
            publish(Model, Verb, Path, Msg, Context)
    end.

-spec publish( model_name(), verb(), path(), opt_message(), z:context() ) -> {ok, term()} | {error, term()}.
publish(Model, Verb, Path, undefined, Context) ->
    Msg = #{
        type => publish,
        payload => undefined
    },
    publish(Model, Verb, Path, Msg, Context);
publish(Model, Verb, Path, Msg, Context) ->
    ModelTopic = mqtt_topic(Model, Verb, Path),
    {ok, RespTopic} = z_mqtt:temp_response_topic(Context),
    Props = maps:get(path_element, Msg, #{}),
    Msg1 = Msg#{
        topic => ModelTopic,
        properties => Props#{
            response_topic => RespTopic
        }
    },
    case z_mqtt:publish(Msg1, Context) of
        ok ->
            case z_mqtt:await_response(RespTopic, ?MQTT_CALL_TIMEOUT, Context) of
                {ok, #{ message := ReplyMsg } } ->
                    {ok, maps:get(payload, ReplyMsg, undefined)};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.


-spec callback( model_name(), verb(), path(), mqtt_packet_map:mqtt_message(), z:context() ) -> {ok, term()} | {error, term()}.
callback(Model, Verb, Path, Msg, Context) ->
    case get_module(Model, Context) of
        {ok, Mod} ->
            maybe_resolve(Verb, model_call(Mod, map_verb(Verb), Path, Msg, Context), Context);
        {error, _} = Error ->
            ?LOG_NOTICE(#{
                text => <<"Publish to unknown model">>,
                in => zotonic_core,
                model => Model,
                verb => Verb,
                path => Path,
                message => Msg
            }),
            Error
    end.

%% @doc Find the model module in the module index.
%%      The model must be either an atom or a binary.
-spec get_module( atom() | binary(), z:context() ) -> {ok, module()} | {error, term()}.
get_module(rsc, _Context)       -> {ok, m_rsc};
get_module(acl, _Context)       -> {ok, m_acl};
get_module(config, _Context)    -> {ok, m_config};
get_module(edge, _Context)      -> {ok, m_edge};
get_module(hierarchy, _Context) -> {ok, m_hierarchy};
get_module(identity, _Context)  -> {ok, m_identity};
get_module(media, _Context)     -> {ok, m_media};
get_module(modules, _Context)   -> {ok, m_modules};
get_module(mqtt_ticket, _Context)   -> {ok, m_mqtt_ticket};
get_module(predicate, _Context) -> {ok, m_predicate};
get_module(req, _Context)       -> {ok, m_req};
get_module(rsc_gone, _Context)  -> {ok, m_rsc_gone};
get_module(rsc_import, _Context)  -> {ok, m_rsc_import};
get_module(search, _Context)    -> {ok, m_search};
get_module(site, _Context)      -> {ok, m_site};
get_module(sysconfig, _Context) -> {ok, m_sysconfig};
get_module(template, _Context)  -> {ok, m_template};
get_module(<<"rsc">>, _Context)       -> {ok, m_rsc};
get_module(<<"acl">>, _Context)       -> {ok, m_acl};
get_module(<<"config">>, _Context)    -> {ok, m_config};
get_module(<<"edge">>, _Context)      -> {ok, m_edge};
get_module(<<"hierarchy">>, _Context) -> {ok, m_hierarchy};
get_module(<<"identity">>, _Context)  -> {ok, m_identity};
get_module(<<"media">>, _Context)     -> {ok, m_media};
get_module(<<"modules">>, _Context)   -> {ok, m_modules};
get_module(<<"mqtt_ticket">>, _Context)   -> {ok, m_mqtt_ticket};
get_module(<<"predicate">>, _Context) -> {ok, m_predicate};
get_module(<<"req">>, _Context)       -> {ok, m_req};
get_module(<<"rsc_gone">>, _Context)  -> {ok, m_rsc_gone};
get_module(<<"rsc_import">>, _Context)  -> {ok, m_rsc_import};
get_module(<<"search">>, _Context)    -> {ok, m_search};
get_module(<<"site">>, _Context)      -> {ok, m_site};
get_module(<<"sysconfig">>, _Context) -> {ok, m_sysconfig};
get_module(<<"template">>, _Context)  -> {ok, m_template};
get_module(Name, Context) ->
    case z_module_indexer:find(model, Name, Context) of
        {ok, #module_index{ erlang_module = M }} ->
            {ok, M};
        {error, _} = Error ->
            Error
    end.

-spec get_arg( atom(), map() | undefined, z:context() ) -> term() | undefined.
get_arg(Prop, Map, _Context) when is_atom(Prop), is_map(Map) ->
    case maps:find(payload, Map) of
        {ok, Payload} when is_map(Payload) ->
            case maps:find(Prop, Payload) of
                {ok, V} -> V;
                error -> maps:get(atom_to_binary(Prop, utf8), Payload, undefined)
            end;
        _ ->
            case maps:find(Prop, Map) of
                {ok, V} -> V;
                error -> maps:get(atom_to_binary(Prop, utf8), Map, undefined)
            end
    end;
get_arg(Prop, undefined, Context) when is_atom(Prop) ->
    z_context:get_q(Prop, Context).


%%% ------------------------------ Internal functions ---------------------------


%% @doc Return the topic for a model call. The topic has the format: model/mymodel/get/foo/bar
-spec mqtt_topic( atom() | binary(), verb(), path() ) -> mqtt_sessions:topic().
mqtt_topic(Model, Method, Path) ->
    [ <<"model">>, z_convert:to_binary(Model), z_convert:to_binary(Method) | binarize(Path) ].

% Map a verb to an model function.
-spec map_verb( verb() ) -> model_callback().
map_verb(get) -> m_get;
map_verb(post) -> m_post;
map_verb(delete) -> m_delete.


-spec model_call( module(), atom(), path(), mqtt_packet_map:mqtt_message(), z:context() ) -> {ok, term()} | ok | {error, unacceptable | term()}.
model_call(Mod, m_get, Path, Msg, Context) ->
    Mod:m_get(binarize(Path), Msg, Context);
model_call(Mod, Callback, Path, Msg, Context) ->
    try
        Mod:Callback(binarize(Path), Msg, Context)
    catch
        error:function_clause:S ->
            case S of
                [ {Mod, Callback, _As, _Loc} | _ ] ->
                    {error, unknown_path};
                _ ->
                    ?LOG_ERROR(#{
                        text => <<"Error in model function call">>,
                        in => zotonic_core,
                        result => error,
                        reason => function_clause,
                        model => Mod,
                        callback => Callback,
                        path => Path,
                        stack => S
                    }),
                    {error, function_clause}
            end;
        error:undef:S ->
            case S of
                [ {Mod, Callback, _As, _Loc} | _ ] ->
                    {error, unknown_path};
                _ ->
                    ?LOG_ERROR(#{
                        text => <<"Undef in model call">>,
                        in => zotonic_core,
                        result => error,
                        reason => undef,
                        model => Mod,
                        callback => Callback,
                        path => Path,
                        stack => S
                    }),
                    {error, undef}
            end
    end.

-spec binarize( list() ) -> list( binary() | term() ).
binarize(Path) ->
    [ maybe_binary(P) || P <- Path ].

maybe_binary(B) when is_binary(B) -> B;
maybe_binary(undefined) -> <<>>;
maybe_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
maybe_binary(V) -> V.

%% @doc Optionally dig deeper into the returned result if the path is not consumed completely.
maybe_resolve(get, {ok, {Res, []}}, _Context) ->
    {ok, Res};
maybe_resolve(get, {ok, {Res, Ks}}, Context) when is_list(Ks) ->
    Res1 = z_template_compiler_runtime:find_nested_value(Res, Ks, #{}, Context),
    {ok, Res1};
maybe_resolve(_Verb, {ok, Res}, _Context) ->
    {ok, Res};
maybe_resolve(_Verb, ok, _Context) ->
    {ok, <<>>};
maybe_resolve(_Verb, {error, _} = Error, _Context) ->
    Error.
