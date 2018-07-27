%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2018 Marc Worrell
%%
%% @doc Call models, direct or via MQTT

%% Copyright 2018 Marc Worrell
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

    call/5,
    publish/5,
    callback/5,

    get_module/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

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
payload_msg(undefined) ->
    undefined;
payload_msg(null) ->
    undefined;
payload_msg(Payload) ->
    #{
        type => publish,
        topic => [],
        properties => #{},
        payload => Payload
    }.

-spec call( model_name(), verb(), path(), opt_message(), z:context() ) -> {ok, term()} | {error, term()}.
call(Model, Verb, Path, Msg, Context) ->
    case get_module(Model, Context) of
        {ok, Mod} ->
            maybe_resolve(model_call(Mod, map_verb(Verb), Path, Msg, Context), Context);
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
    RespTopic = z_mqtt:temp_response_topic(Context),
    Props = maps:get(Msg, path_element, #{}),
    Msg1 = Msg#{
        topic => ModelTopic,
        properties => Props#{
            response_topic => RespTopic
        }
    },
    case z_mqtt:publish(Msg1, Context) of
        ok ->
            case z_mqtt:await_response(RespTopic, ?MQTT_CALL_TIMEOUT) of
                {ok, {mqtt_msg, #{ message := Msg}}} ->
                    {ok, maps:get(payload, Msg, undefined)};
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
            maybe_resolve(model_call(Mod, map_verb(Verb), Path, Msg, Context), Context);
        {error, _} = Error ->
            lager:info("Publish to unknown model ~p", [Model]),
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
get_module(predicate, _Context) -> {ok, m_predicate};
get_module(req, _Context)       -> {ok, m_req};
get_module(rsc_gone, _Context)  -> {ok, m_rsc_gone};
get_module(search, _Context)    -> {ok, m_search};
get_module(site, _Context)      -> {ok, m_site};
get_module(sysconfig, _Context) -> {ok, m_sysconfig};
get_module(Name, Context) ->
    case z_module_indexer:find(model, Name, Context) of
        {ok, #module_index{ erlang_module = M }} ->
            {ok, M};
        {error, _} = Error ->
            Error
    end.


%%% ------------------------------ Internal functions ---------------------------


%% @doc Return the topic for a model call. The topic has the format: m/mymodel/get/foo/bar
-spec mqtt_topic( atom() | binary(), verb(), path() ) -> mqtt_sessions:topic().
mqtt_topic(Model, Method, Path) ->
    % Path = cowmachine_req:disp_path(Context),
    % Parts = binary:split(Path, <<"/">>, [global]),
    % Parts1 = lists:map( fun cow_qs:urldecode/1, Parts ),
    Path1 = [ z_convert:to_binary(P) || P <- Path ],
    [ <<"m">>, z_convert:to_binary(Model), z_convert:to_binary(Method) | Path1 ].



% Map a verb to an model function.
-spec map_verb( verb() ) -> model_callback().
map_verb(get) -> m_get;
map_verb(post) -> m_post;
map_verb(delete) -> m_delete.


-spec model_call( module(), atom(), path(), mqtt_packet_map:mqtt_message(), z:context() ) -> {ok, term()} | {error, unacceptable | term()}.
model_call(Mod, m_get, Path, Msg, Context) ->
    Mod:m_get(atomize(Path), Msg, Context);
model_call(Mod, Callback, Path, Msg, Context) ->
    try
        Mod:Callback(atomize(Path), Msg, Context)
    catch
        error:undef ->
            case erlang:function_exported(Mod, Callback, 3) of
                false ->
                    lager:info("Model ~p does not export ~p", [ Mod, Callback ]),
                    {error, unacceptable};
                true ->
                    erlang:raise(error, undef, erlang:get_stacktrace())
            end
    end.

atomize(Path) ->
    [ maybe_atom(P) || P <- Path ].

maybe_atom(P) when is_binary(P) ->
    try binary_to_existing_atom(P, utf8)
    catch error:badarg -> P
    end;
maybe_atom(V) ->
    V.

%% @doc Optionally dig deeper into the returned result if the path is not consumed completely.
maybe_resolve({ok, {Res, []}}, _Context) ->
    {ok, Res};
maybe_resolve({ok, {Res, Ks}}, Context) when is_list(Ks) ->
    Res1 = z_template_compiler_runtime:find_nested_value(Res, Ks, #{}, Context),
    {ok, Res1};
maybe_resolve({error, _} = Error, _Context) ->
    Error.
