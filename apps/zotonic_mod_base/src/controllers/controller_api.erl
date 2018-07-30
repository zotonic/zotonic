%% @author Arjan Scherpenisse <arjan@scherpenisse.net>, Marc Worrell <marc@worrell.nl>
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
    resource_exists/1,
    allowed_methods/1,
    content_types_provided/1,
    content_types_accepted/1,

    process/4
]).

% Default max body length (32MB) for API calls, this should be configurable.
-define(MAX_BODY_LENGTH, 32*1024*1024).

-include_lib("zotonic_core/include/zotonic.hrl").

service_available(Context) ->
    Context1 = set_cors_header(Context),
    {true, Context1}.

% Headers where already added in service_available/2
options(Context) ->
    {[], Context}.

allowed_methods(Context) ->
    {[ <<"GET">>, <<"POST">>, <<"DELETE">> ], Context}.

resource_exists(Context) ->
    case z_context:get(service_module, Context) of
		undefined -> {false, Context};
		_ServiceModule -> {true, Context}
    end.

%% @doc Content types accepted for the post body
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
content_types_provided(Context) ->
    {[
        {<<"application">>, <<"json">>, []},
        {<<"application">>, <<"javascript">>, []},
        {<<"text">>, <<"javascript">>, []},
        {<<"text">>, <<"x-ubf">>, []},
        {<<"application">>, <<"x-bert">>, []}
     ], Context}.

%% @doc Process the request, call MQTT and reply with the response
process(Method, AcceptedCT, ProvidedCT, Context) ->
    {Payload, Context1} = payload(AcceptedCT, Context),
    {Model, Path} = request_model_path(Context),
    Msg = #{
        type => publish,
        properties => #{
            content_type => AcceptedCT
        },
        payload => Payload
    },
    case z_model:call(map_method(Method), Model, Path, Msg, Context) of
        {ok, Resp} ->
            Body = encode(ProvidedCT, Resp),
            {Body, Context1};
        {error, _} = Error ->
            error_result(Error, ProvidedCT, Context)
    end.

map_method(<<"GET">>) -> get;
map_method(<<"HEAD">>) -> get;
map_method(<<"POST">>) -> post;
map_method(<<"DELETE">>) -> delete.

payload(undefined, Context) ->
    from_qs(Context);
payload(<<"application/json">>, Context) ->
    from_json(Context);
payload(<<"application/javascript">>, Context) ->
    from_json(Context);
payload(<<"text/javascript">>, Context) ->
    from_json(Context);
payload(<<"text/x-ubf">>, Context) ->
    {Body, Context1} = req_body(Context),
    {Data, _Rest} = z_ubf:decode(Body),
    {Data, Context1};
payload(<<"application/x-www-form-urlencoded">>, Context) ->
    from_qs(Context);
payload(<<"multipart/form-data">>, Context) ->
    from_qs(Context);
payload(_CT, Context) ->
    req_body(Context).

%% @doc Decode the incoming body
from_json(Context) ->
    {Body, Context1} = req_body(Context),
    Data = jsxrecord:decode(Body),
    {Data, Context1}.

%% @doc Decode the incoming body
from_qs(Context) ->
    Context1 = z_context:ensure_qs(Context),
    Qs = z_context:get_q_all_noz(Context1),
    {Qs, Context1}.

encode(<<"application/json">>, Data) ->
    jsxrecord:encode(Data);
encode(<<"application/javascript">>, Data) ->
    jsxrecord:encode(Data);
encode(<<"text/javascript">>, Data) ->
    jsxrecord:encode(Data);
encode(<<"text/x-ubf">>, Data) ->
    z_ubf:encode(Data);
encode(<<"application/x-bert">>, Data) ->
    erlang:term_to_binary(Data).


%% @doc Return the model and path to be called.
-spec request_model_path( z:context() ) -> {binary(), list( binary() )}.
request_model_path(Context) ->
    Path = cowmachine_req:disp_path(Context),
    Parts = binary:split(Path, <<"/">>, [global]),
    Parts1 = lists:map( fun cow_qs:urldecode/1, Parts ),
    {hd(Parts1), tl(Parts1)}.


-spec req_body( z:context() ) -> binary().
req_body(Context) ->
    cowmachine_req:req_body(?MAX_BODY_LENGTH, Context).


error_result({error, eacces}, CT, Context) ->
    RespBody = encode(CT, #{
            status => <<"error">>,
            error => <<"eacces">>,
            message => <<"Access Denied">>
        }),
    Context1 = cowmachine_req:set_resp_body(RespBody, Context),
    {{halt, 403}, Context1};
error_result({error, payload}, CT, Context) ->
    RespBody = encode(CT, #{
            status => <<"error">>,
            error => <<"payload">>,
            message => <<"Illegal Payload Encoding">>
        }),
    Context1 = cowmachine_req:set_resp_body(RespBody, Context),
    {{halt, 400}, Context1};
error_result({error, StatusCode}, CT, Context) when is_integer(StatusCode) ->
    RespBody = encode(CT, #{
            status => <<"error">>,
            error => <<"error">>,
            message => <<"Error ", (integer_to_binary(StatusCode))/binary>>
        }),
    Context1 = cowmachine_req:set_resp_body(RespBody, Context),
    {{halt, StatusCode}, Context1};
error_result({error, {StatusCode, Reason, Message}}, CT, Context) when is_integer(StatusCode) ->
    RespBody = encode(CT, #{
            status => <<"error">>,
            error => z_convert:to_binary(Reason),
            message => z_convert:to_binary(Message)
        }),
    Context1 = cowmachine_req:set_resp_body(RespBody, Context),
    {{halt, StatusCode}, Context1};
error_result({error, Reason}, CT, Context) ->
    RespBody = encode(CT, #{
            status => <<"error">>,
            error => z_convert:to_binary(Reason),
            message => <<"Internal Error">>
        }),
    Context1 = cowmachine_req:set_resp_body(RespBody, Context),
    {{halt, 500}, Context1};
error_result(_, CT, Context) ->
    RespBody = encode(CT, #{
            status => <<"error">>,
            error => <<"error">>,
            message => <<"Internal Error">>
        }),
    Context1 = cowmachine_req:set_resp_body(RespBody, Context),
    {{halt, 500}, Context1}.


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
