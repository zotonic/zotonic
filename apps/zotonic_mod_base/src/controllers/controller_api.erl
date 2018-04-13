%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2009-09-27
%% @doc Entrypoint for API requests.

%% Copyright 2009 Arjan Scherpenisse
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

-export([
    service_available/1,
    options/1,
    resource_exists/1,
    allowed_methods/1,
    process_post/1,
    is_authorized/1,
    content_types_provided/1,
    to_json/1,
    get_q_all/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

service_available(Context) ->
    Context1 = set_cors_header(Context),
    {true, Context1}.

% Headers where already added in service_available/2
options(Context) ->
    {[], Context}.

allowed_methods(Context) ->
    % Continue session for API calls from a web page
    Context1 = z_context:ensure_qs(Context),
    z_context:lager_md(Context1),
    Module = case z_context:get_q(<<"api_module">>, Context1) of
                 undefined -> z_convert:to_binary(z_context:get(module, Context1));
                 Mod -> Mod
             end,
    Method = case z_context:get_q(<<"api_method">>, Context1) of
                 undefined ->
                     case z_convert:to_binary(z_context:get(method, Context1)) of
                         <<>> -> Module;
                         Mtd -> Mtd
                     end;
                 Mtd -> Mtd
             end,
    case ensure_existing_module(Module, Method, Context1) of
        {ok, ServiceModule} ->
            Context2 = z_context:set(service_module, ServiceModule, Context1),
            {[<<"OPTIONS">> | z_service:http_methods(ServiceModule)], Context2};
        {error, enoent} ->
            Context2 = z_context:set(service_module, undefined, Context1),
            {[<<"OPTIONS">>, <<"GET">>, <<"HEAD">>, <<"POST">>], Context2}
    end.

ensure_existing_module(Module, Method, Context) ->
    case z_module_indexer:find(service, {Module, Method}, Context) of
        {ok, #module_index{erlang_module=ServiceModule}} ->
            {ok, ServiceModule};
        {error, _} = Error ->
            Error
    end.

is_authorized(Context) ->
    case z_context:get(service_module, Context) of
        undefined -> {true, Context};
        Module ->
            case z_service:needauth(Module) of
                false ->
                    {true, Context};
                true ->
                    case z_auth:is_auth(Context) of
                        true ->
                            {true, Context};
                        false ->
                            case z_notifier:first(#service_authorize{service_module=Module}, Context) of
                                undefined ->
                                    api_error(500, 0, "No service authorization method available", [], Context);
                                Reply ->
                                    Reply
                            end
                    end
            end
    end.

resource_exists(Context) ->
    case z_context:get(service_module, Context) of
		undefined -> {false, Context};
		_ServiceModule -> {true, Context}
    end.

content_types_provided(Context) ->
    {[{<<"application/json">>, to_json},
      {<<"application/javascript">>, to_json},
      {<<"text/javascript">>, to_json}
     ], Context}.


%% @doc Called for 'GET' requests
to_json(Context) ->
    Module = z_context:get(service_module, Context),
    try
        case Module:process_get(Context) of
            {R, C=#context{}} -> api_result(R, C);
            R -> api_result(R, Context)
        end
    catch
        throw:{error, _, _} = R1 ->
            api_result(Context, R1);
        throw:{error, _, _, _} = R2 ->
            api_result(Context, R2);
        E:R3 ->
            StackTrace = erlang:get_stacktrace(),
            lager:error("controller_api error: ~p:~p - ~p", [E, R3, StackTrace]),
            api_result(Context, {error, internal_server_error, []})
    end.



%% @doc Called for 'POST' requests
process_post(Context0) ->
    case handle_json_request(Context0) of
        {error, _Reason, Context} ->
            api_result(Context, {error, syntax, "invalid JSON in request body"});
        {ok, Context1} ->
            Module = z_context:get(service_module, Context1),
            try
                case Module:process_post(Context1) of
                    ok ->
                        {true, Context1};
                    {Result, Context2=#context{}} ->
                        api_result(Result, Context2);
                    Result ->
                        api_result(Result, Context1)
                end
            catch
                throw:{error, _, _} = R1 ->
                    api_result(Context1, R1);
                throw:{error, _, _, _} = R2 ->
                    api_result(Context1, R2);
                E:R ->
                    lager:error("controller_api error: ~p:~p - ~p", [E, R, erlang:get_stacktrace()]),
                    api_result(Context1, {error, internal_server_error, []})
            end
    end.


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


api_error(HttpCode, ErrCode, Message, ErrData, Context) ->
    Error = #{
        <<"error">> => #{
            <<"code">> => ErrCode,
            <<"message">> => Message,
            <<"data">> => ErrData
        }
    },
    Body = z_json:encode(Error),
    {{halt, HttpCode}, cowmachine_req:set_resp_body(Body, Context)}.


api_result({error, Err, Arg}, Context) ->
    api_result({error, Err, Arg, #{}}, Context);
api_result({error, Err, Arg, ErrData}, Context) when is_list(Arg) ->
    api_result({error, Err, list_to_binary(Arg), ErrData}, Context);
api_result({error, Err=missing_arg, Arg, ErrData}, Context) ->
    api_error(400, Err, <<"Missing argument: ", Arg/binary>>, ErrData, Context);
api_result({error, Err=unknown_arg, Arg, ErrData}, Context) ->
    api_error(400, Err, <<"Unknown argument: ", Arg/binary>>, ErrData, Context);
api_result({error, Err=syntax, Arg, ErrData}, Context) ->
    api_error(400, Err, <<"Syntax error: ", Arg/binary>>, ErrData, Context);
api_result({error, Err=unauthorized, _Arg, ErrData}, Context) ->
    api_error(401, Err, <<"Unauthorized.">>, ErrData, Context);
api_result({error, Err=not_exists, Arg, ErrData}, Context) ->
    api_error(404, Err, <<"Resource does not exist: ", Arg/binary>>, ErrData, Context);
api_result({error, Err=access_denied, _Arg, ErrData}, Context) ->
    api_error(403, Err, <<"Access denied.">>, ErrData, Context);
api_result({error, Err=unprocessable, Arg, ErrData}, Context) ->
    api_error(422, Err, <<"Unprocessable entity: ", Arg/binary>>, ErrData, Context);
api_result({error, Err, _Arg, ErrData}, Context) ->
    api_error(500, Err, <<"Generic error.">>, ErrData, Context);
api_result(Result, Context) ->
    try
        JSON = result_to_json(Result),
        Body = case get_callback(Context) of
                   undefined -> JSON;
                   Callback -> [Callback, $(, JSON, $), $;]
               end,
        {{halt, 200}, cowmachine_req:set_resp_body(Body, Context)}
    catch
        E:R ->
            StackTrace = erlang:get_stacktrace(),
            lager:error("controller_api error: ~p:~p - ~p", [E, R, StackTrace]),
            Context1 = cowmachine_req:set_resp_body(<<"Internal JSON encoding error.\n">>, Context),
            {{halt, 500}, Context1}
    end.

result_to_json(B) when is_binary(B) -> B;
result_to_json(R) -> z_json:encode(R).


%% @doc Handle JSON request bodies.
-spec handle_json_request(#context{}) -> {ok, #context{}} | {error, term(), #context{}}.
handle_json_request(Context) ->
    case z_context:get_req_header(<<"content-type">>, Context) of
        <<"application/json", _/binary>> ->
            decode_json_body(Context);
        _ ->
            {ok, Context}
    end.

%% @doc Decode JSON request body.
-spec decode_json_body(#context{}) -> {ok, #context{}} | {error, term(), #context{}}.
decode_json_body(Context0) ->
    {ReqBody, Context} = cowmachine_req:req_body(Context0),
    case ReqBody of
        <<>> ->
            {ok, Context};
        NonEmptyBody ->
            try
                case z_json:decode(NonEmptyBody) of
                    {error, Error} ->
                        {error, Error, Context};
                    Json when is_list(Json) ->
                        %% A JSON list: don't alter context
                        {ok, Context};
                    Json ->
                        %% A JSON object: set key/value pairs in the context
                        Context1 = maps:fold(
                            fun(Key, Value, Context2) ->
                                z_context:set_q(Key, Value, Context2)
                            end,
                            Context,
                            Json
                        ),
                        {ok, Context1}
                end
            catch
                Type:Reason ->
                    {error, {Type, Reason}, Context}
            end
    end.

get_callback(Context) ->
    case z_context:get_q(<<"callback">>, Context) of
        undefined ->
            case z_context:get_q(<<"jsonp">>, Context) of
                undefined -> filter(z_context:get_q(<<"jsoncallback">>, Context));
                Callback -> filter(Callback)
            end;
        Callback ->
            filter(Callback)
    end.

get_q_all(Context) ->
    proplists:delete(<<"module">>,
    proplists:delete(<<"method">>,
    proplists:delete(<<"jsoncallback">>,
    proplists:delete(<<"callback">>,
    proplists:delete(<<"jsonp">>,
        z_context:get_q_all_noz(Context)))))).

filter(undefined) ->
    undefined;
filter(F) when is_binary(F) ->
    F1 = binary_to_list(F),
    list_to_binary([
       C || C <- F1,    (C >= $0 andalso C =< $9)
                 orelse (C >= $a andalso C =< $z)
                 orelse (C >= $A andalso C =< $Z)
                 orelse C =:= $_
                 orelse C =:= $.
                 orelse C =:= $[
                 orelse C =:= $]
                 orelse C =:= $$
    ]).
