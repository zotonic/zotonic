%% @author Arjan Scherpenisse, Marc Worrell
%% @copyright 2009-2018 Arjan Scherpenisse <arjan@scherpenisse.net>, Marc Worrell <marc@worrell.nl>
%% @doc Entrypoint for model requests via HTTP.

%% Copyright 2009-2018 Arjan Scherpenisse, Marc Worrell
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

-module(controller_api).

-author("Arjan Scherpenisse <arjan@scherpenisse.net>").
-author("Marc Worrell <marc@worrell.nl>").

-export([
    service_available/1,
    options/1,
    allowed_methods/1,
    malformed_request/1,
    content_types_provided/1,
    content_types_accepted/1,

    process/4
]).

% Default max body length (32MB) for API calls, this should be configurable.
-define(MAX_BODY_LENGTH, 32*1024*1024).

-define(is_http_status(Code), is_integer(Code), Code >= 200, Code < 600).

-include_lib("zotonic_core/include/zotonic.hrl").

-spec service_available( z:context() ) -> {boolean(), z:context()}.
service_available(Context) ->
    Context1 = set_cors_header(Context),
    {true, Context1}.

% Headers where already added in service_available/2
-spec options( z:context() ) -> {list( binary() ), z:context()}.
options(Context) ->
    {[], Context}.

-spec allowed_methods( z:context() ) -> {[ binary() ], z:context()}.
allowed_methods(Context) ->
    {[ <<"GET">>, <<"POST">>, <<"DELETE">> ], Context}.

-spec malformed_request( z:context() ) -> {boolean(), z:context()}.
malformed_request(Context) ->
    Path = cow_qs:urldecode( cowmachine_req:disp_path(Context) ),
    case mqtt_packet_map_topic:validate_topic(Path) of
        {ok, Topic} ->
            Context1 = z_context:set(topic, Topic, Context),
            Method = cowmachine_req:method(Context1),
            {not is_method_topic_match(Method, Topic), Context1};
        {error, _} ->
            {true, Context}
    end.

% Check method against model call methods
% GET -> get
% POST -> post / delete / get
% DELETE -> delete
is_method_topic_match(<<"GET">>, [ <<"model">>, _Model, <<"get">> | _ ]) -> true;
is_method_topic_match(<<"GET">>, [ <<"model">>, _Model, _ModelMethod | _ ]) -> false;
is_method_topic_match(<<"GET">>, _) -> true;
is_method_topic_match(<<"DELETE">>, [ <<"model">>, _Model, <<"delete">> | _ ]) -> true;
is_method_topic_match(<<"DELETE">>, _) -> false;
is_method_topic_match(<<"POST">>, _) -> true.


%% @doc Content types accepted for the post body
-spec content_types_accepted( z:context() ) -> {list( cowmachine_req:mime_type() ), z:context()}.
content_types_accepted(Context) ->
    {[
        {<<"application">>, <<"json">>, []},
        {<<"application">>, <<"javascript">>, []},
        {<<"text">>, <<"javascript">>, []},
        {<<"text">>, <<"x-ubf">>, []},
        {<<"application">>, <<"x-bert">>, []},
        {<<"application">>, <<"x-www-form-urlencoded">>, []},
        {<<"multipart">>, <<"form-data">>, []}
    ], Context}.

%% @doc Content types provided for the resulting body
-spec content_types_provided( z:context() ) -> {list( cowmachine_req:mime_type() ), z:context()}.
content_types_provided(Context) ->
    {[
        {<<"application">>, <<"json">>, []},
        {<<"application">>, <<"javascript">>, []},
        {<<"text">>, <<"javascript">>, []},
        {<<"text">>, <<"x-ubf">>, []},
        {<<"application">>, <<"x-bert">>, []},
        {<<"text">>, <<"event-stream">>, []}
     ], Context}.

%% @doc Process the request, call MQTT and reply with the response
-spec process( binary(), cowmachine_req:media_type() | undefined, cowmachine_req:media_type(), z:context() )
        -> {iodata(), z:context()} | {{halt, HttpCode :: pos_integer()}, z:context()}.
process(_Method, _AcceptedCT, {<<"text">>, <<"event-stream">>, _}, Context) ->
    case z_mqtt:subscribe(z_context:get(topic, Context), Context) of
        ok ->
            event_stream(Context);
        {error, _} = Error ->
            process_done(Error, {<<"application">>, <<"json">>, []}, Context)
    end;
process(_Method, AcceptedCT, ProvidedCT, Context) ->
    {Payload, Context1} = z_controller_helper:decode_request(AcceptedCT, Context),
    case z_context:get_q(<<"response_topic">>, Context) of
        undefined ->
            Msg = #{
                type => publish,
                topic => z_context:get(topic, Context1),
                properties => #{
                    content_type => AcceptedCT
                },
                payload => Payload,
                qos => 0,
                retain => false
            },
            process_done( z_mqtt:call(Msg, Context1), ProvidedCT, Context1 );
        <<>> ->
            Msg = #{
                type => publish,
                topic => z_context:get(topic, Context1),
                properties => #{
                    content_type => AcceptedCT
                },
                payload => Payload,
                qos => 0,
                retain => false
            },
            process_done( z_mqtt:publish(Msg, Context1), ProvidedCT, Context1);
        RespTopic ->
            case mqtt_packet_map_topic:validate_topic_publish(RespTopic) of
                {ok, RespTopic1} ->
                    Msg = #{
                        type => publish,
                        topic => z_context:get(topic, Context1),
                        properties => #{
                            content_type => AcceptedCT,
                            response_topic => RespTopic1
                        },
                        payload => Payload,
                        qos => 0,
                        retain => false
                    },
                    process_done( z_mqtt:publish(Msg, Context1), ProvidedCT, Context1);
                {error, _} = Error ->
                    process_done( Error, ProvidedCT, Context1)
            end
    end.

-spec process_done( ok | {ok, term()}  | {error, term()}, cowmachine_req:media_type(), z:context() ) ->
        {iodata(), z:context()} | {{halt, HttpCode :: pos_integer()}, z:context()}.
process_done(ok, ProvidedCT, Context) ->
    % z_mqtt:publish response
    Body = z_controller_helper:encode_response(ProvidedCT, #{ status => ok }),
    {Body, Context};
process_done({ok, #{
        status := <<"error">>,
        error := _
    } = Resp}, ProvidedCT, Context) ->
    Body = z_controller_helper:encode_response(ProvidedCT, Resp),
    {Body, Context};
process_done({ok, Resp}, ProvidedCT, Context) ->
    % z_mqtt:call response
    Body = z_controller_helper:encode_response(ProvidedCT, Resp),
    {Body, Context};
process_done({error, _} = Error, ProvidedCT, Context) ->
    error_response(Error, ProvidedCT, Context).

-spec error_response({error, term()}, cowmachine_req:media_type(), z:context()) ->
    {{halt, HttpCode :: pos_integer()}, z:context()}.
error_response({error, payload}, CT, Context) ->
    RespBody = z_controller_helper:encode_response(CT, #{
            status => <<"error">>,
            error => <<"payload">>,
            message => <<"Illegal Payload Encoding">>
        }),
    Context1 = cowmachine_req:set_resp_body(RespBody, Context),
    {{halt, 400}, Context1};
error_response({error, eacces}, CT, Context) ->
    RespBody = z_controller_helper:encode_response(CT, #{
            status => <<"error">>,
            error => <<"eacces">>,
            message => <<"Access Denied">>
        }),
    Context1 = cowmachine_req:set_resp_body(RespBody, Context),
    {{halt, 403}, Context1};
error_response({error, enoent}, CT, Context) ->
    RespBody = z_controller_helper:encode_response(CT, #{
            status => <<"error">>,
            error => <<"enoent">>,
            message => <<"Not Found">>
        }),
    Context1 = cowmachine_req:set_resp_body(RespBody, Context),
    {{halt, 404}, Context1};
error_response({error, unknown_path}, CT, Context) ->
    RespBody = z_controller_helper:encode_response(CT, #{
            status => <<"error">>,
            error => <<"unknown_path">>,
            message => <<"Not Found">>
        }),
    Context1 = cowmachine_req:set_resp_body(RespBody, Context),
    {{halt, 404}, Context1};
error_response({error, unacceptable}, CT, Context) ->
    RespBody = z_controller_helper:encode_response(CT, #{
            status => <<"error">>,
            error => <<"unacceptable">>,
            message => <<"Unacceptable Model Method">>
        }),
    Context1 = cowmachine_req:set_resp_body(RespBody, Context),
    {{halt, 400}, Context1};
error_response({error, StatusCode}, CT, Context) when ?is_http_status(StatusCode) ->
    RespBody = z_controller_helper:encode_response(CT, #{
            status => <<"error">>,
            error => StatusCode,
            message => <<"Error ", (integer_to_binary(StatusCode))/binary>>
        }),
    Context1 = cowmachine_req:set_resp_body(RespBody, Context),
    {{halt, StatusCode}, Context1};
error_response({error, Reason}, CT, Context) ->
    RespBody = z_controller_helper:encode_response(CT, #{
            status => <<"error">>,
            error => Reason,
            message => <<"Internal Error">>
        }),
    Context1 = cowmachine_req:set_resp_body(RespBody, Context),
    {{halt, 500}, Context1}.


%% @doc Event stream with messages sent to a subscribed topic
event_stream(Context) ->
    % TODO: add streaming body function
    {{halt, 501}, Context}.


%% set in site config file
%%  [{service_api_cors, false}, %% 2nd is default value
%%   {'Access-Control-Allow-Origin', "*"},
%%   {'Access-Control-Allow-Credentials', undefined},
%%   {'Access-Control-Max-Age', undefined},
%%   {'Access-Control-Allow-Methods', undefined},
%%   {'Access-Control-Allow-Headers', undefined}]
set_cors_header(Context) ->
    set_cors_header(z_convert:to_bool(m_site:get(service_api_cors, Context)), Context).

set_cors_header(true, Context) ->
    lists:foldl(
            fun ({K, Header, Def}, Ctx) ->
                case m_site:get(K, Ctx) of
                    undefined when Def =/= undefined ->
                        z_context:set_resp_header(Header, Def, Ctx);
                    undefined when Def =:= undefined ->
                        Ctx;
                    V ->
                        z_context:set_resp_header(Header, z_convert:to_binary(V), Ctx)
                end
            end,
            Context,
            [{'Access-Control-Allow-Origin', <<"access-control-allow-origin">>, <<"*">>},
             {'Access-Control-Allow-Credentials', <<"access-control-allow-credentials">>, undefined},
             {'Access-Control-Max-Age', <<"access-control-max-age">>, undefined},
             {'Access-Control-Allow-Methods', <<"access-control-allow-methods">>, undefined},
             {'Access-Control-Allow-Headers', <<"access-control-allow-headers">>, undefined}]);
set_cors_header(false, Context) ->
    Context.
