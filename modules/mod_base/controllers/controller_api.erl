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
    init/1,
    service_available/2,
    resource_exists/2,
    allowed_methods/2,
    process_post/2,
    is_authorized/2,
    content_types_provided/2,
    delete_resource/2,
    to_json/2,
    get_q_all/1
]).

-include_lib("controller_webmachine_helper.hrl").
-include_lib("zotonic.hrl").

init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context1 = z_context:new_request(ReqData, DispatchArgs, ?MODULE),
    ReqData1 = set_cors_header(ReqData, Context1),
    case wrq:method(ReqData1) of
        'OPTIONS' ->
            {{halt, 204}, ReqData1, Context1};
        _ ->
            {true, ReqData1, Context1}
    end.


allowed_methods(ReqData, Context) ->
    Context0 = ?WM_REQ(ReqData, Context),
    Context1 = z_context:ensure_qs(z_context:continue_session(Context0)),
    z_context:lager_md(Context1),
    TheMod = case z_context:get_q("module", Context1) of
                 undefined -> z_convert:to_list(z_context:get(module, Context1));
                 M -> M
             end,
    Method = case z_context:get_q("method", Context1) of
                 undefined ->
                     case z_convert:to_list(z_context:get(method, Context1)) of
                         [] -> TheMod; %% method == module name
                         M2 -> M2
                     end;
                 M3 -> M3
             end,
    case ensure_existing_module("service_" ++ TheMod ++ "_" ++ Method) of
        {ok, Module} ->
            Context2 = z_context:set(service_module, Module, Context1),
            try
                {['OPTIONS' | z_service:http_methods(Module)], ReqData, Context2}
            catch
                _X:_Y ->
                    {['GET', 'HEAD', 'POST', 'OPTIONS'], ReqData, Context2}
            end;
        {error, not_found} ->
            %% The atom (service module) does not exist, return default methods.
            {['GET', 'HEAD', 'POST', 'OPTIONS'], ReqData, Context1}
    end.

ensure_existing_module(ModuleName) ->
    try
        {ok, list_to_existing_atom(ModuleName)}
    catch
        error:badarg ->
            z_utils:ensure_existing_module(ModuleName)
    end.

is_authorized(ReqData, Context) ->
    %% Check if we are authorized via a regular session.
    Context0 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:ensure_qs(z_context:continue_session(Context0)),
    Module = z_context:get(service_module, Context2),
    case z_service:needauth(Module) of
        false ->
            %% No auth needed; so we're authorized.
            {true, ReqData, Context2};
        true ->
            %% Auth needed; see if we're authorized through regular session
            case z_auth:is_auth(Context2) of
                true ->
                    %% Yep; use these credentials.
                    ?WM_REPLY(true, Context2);
                false ->
                    %% No; see if we can use OAuth.
                    case z_notifier:first(#service_authorize{service_module=Module}, Context2) of
                        undefined ->
                            api_error(500, 0, "No service authorization method available", [], ReqData, Context2);
                        Reply ->
                            Reply
                    end
            end
    end.


resource_exists(ReqData, Context) ->
    Exists = case z_context:get(service_module, Context) of
		 undefined -> false;
		 ServiceModule ->
		     case z_service:serviceinfo(ServiceModule, Context) of
			 undefined -> false;
			 _ -> true
		     end
	     end,
    {Exists, ReqData, Context}.


content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json},
      {"application/javascript", to_json},
      {"text/javascript", to_json}
     ], ReqData, Context}.


%% set in site config file
%%  [{service_api_cors, false}, %% 2nd is default value
%%   {'Access-Control-Allow-Origin', "*"},
%%   {'Access-Control-Allow-Credentials', undefined},
%%   {'Access-Control-Max-Age', undefined},
%%   {'Access-Control-Allow-Methods', undefined},
%%   {'Access-Control-Allow-Headers', undefined}]
set_cors_header(ReqData, Context) ->
    case z_convert:to_bool(m_config:get_value(site, service_api_cors, Context)) of
        true ->
            lists:foldl(
                    fun ({K, Def}, Acc) ->
                        case m_config:get_value(site, K, Def, Context) of
                            undefined ->
                                Acc;
                            V ->
                                wrq:set_resp_header(z_convert:to_list(K), z_convert:to_list(V), Acc)
                        end
                    end,
                    ReqData,
                    [{'Access-Control-Allow-Origin', "*"},
                     {'Access-Control-Allow-Credentials', undefined},
                     {'Access-Control-Max-Age', undefined},
                     {'Access-Control-Allow-Methods', undefined},
                     {'Access-Control-Allow-Headers', undefined}]);
        false ->
            ReqData
    end.


api_result(Context, Result) ->
    ReqData = z_context:get_reqdata(Context),
    case Result of
        {error, Err, Arg, ErrData} ->
            %% ErrData is a JSON structure
            api_result_error(Err, Arg, ErrData, ReqData, Context);
        {error, Err, Arg} ->
            api_result_error(Err, Arg, [], ReqData, Context);
        Result2 ->
            try
                JSON = result_to_json(Result2),
                Body = case get_callback(Context) of
                           undefined -> JSON;
                           Callback -> [ Callback, $(, JSON, $), $; ]
                       end,
                {{halt, 200}, wrq:set_resp_body(Body, ReqData), Context}
            catch
                ?WITH_STACKTRACE(E, R, S)
                    lager:error("controller_api error: ~p:~p - ~p", [E, R, S]),
                    ReqData1 = wrq:set_resp_body("Internal JSON encoding error.\n", ReqData),
                    {{halt, 500}, ReqData1, Context}
            end
    end.

result_to_json(B) when is_binary(B) -> B;
result_to_json(M) when is_map(M) -> jsx:encode(M);
result_to_json({binary_json, R}) -> iolist_to_binary(mochijson:binary_encode(R));
result_to_json(R) -> iolist_to_binary(mochijson:encode(R)).


api_result_error(Err, Arg, ErrData, ReqData, Context) ->
    case Err of
        missing_arg ->
            api_error(400, Err, "Missing argument: " ++ Arg, ErrData, ReqData, Context);
        unknown_arg ->
            api_error(400, Err, "Unknown argument: " ++ Arg, ErrData, ReqData, Context);
        syntax ->
            api_error(400, Err, "Syntax error: " ++ Arg, ErrData, ReqData, Context);
        unauthorized ->
            api_error(401, Err, "Unauthorized.", ErrData, ReqData, Context);
        access_denied ->
            api_error(403, Err, "Access denied.", ErrData, ReqData, Context);
        not_exists ->
            api_error(404, Err, "Resource does not exist: " ++ Arg, ErrData, ReqData, Context);
        % 422: The server was unable to process the contained instructions
        % https://tools.ietf.org/html/rfc4918#section-11.2
        unprocessable ->
            api_error(422, Err, "Unprocessable entity: " ++ Arg, ErrData, ReqData, Context);
        _ ->
            api_error(500, Err, "Generic error.", ErrData, ReqData, Context)
    end.

%% Combines error code and message with user provided error data (if any)
api_error(HttpCode, ErrCode, Message, ErrData, ReqData, Context) ->
    GeneralError = [{error, {struct, [{code, ErrCode}, {message, Message}]}}],
    CombinedError = case ErrData of
        [] -> GeneralError;
        {struct, Data} -> GeneralError ++ Data
    end,
    Error = {struct, CombinedError},
    Body = mochijson:encode(Error),
    {{halt, HttpCode}, wrq:set_resp_body(Body, ReqData), Context}.


to_json(ReqData, Context) ->
    Context0 = ?WM_REQ(ReqData, Context),
    Module = z_context:get(service_module, Context0),
    {Context1, Result} =
        try
            case Module:process_get(ReqData, Context0) of
                {R, C=#context{}} -> {C, R};
                R -> {Context0, R}
            end
        catch
            throw:{error, _, _} = R1 ->
                {Context0, R1};
            throw:{error, _, _, _} = R2 ->
                {Context0, R2};
            ?WITH_STACKTRACE(E, R3, S)
                lager:error("controller_api error: ~p:~p - ~p", [E, R3, S]),
                {Context0, {error, internal_server_error, []}}
        end,
    api_result(Context1, Result).


delete_resource(ReqData, Context) ->
    process_post(ReqData, Context).

process_post(ReqData, Context0) ->
    Context = ?WM_REQ(ReqData, Context0),
    case handle_json_request(ReqData, Context) of
        {error, _Reason} ->
            api_result(Context, {error, syntax, "invalid JSON in request body"});
        {ok, Context1} ->
            Module = z_context:get(service_module, Context1),
            ReqData1 = z_context:get_reqdata(Context1),
            try
                case Module:process_post(ReqData1, Context1) of
                    ok ->
                        {true, ReqData1, Context1};
                    {Result, Context2=#context{}} ->
                        api_result(Context2, Result);
                    Result ->
                        api_result(Context1, Result)
                end
            catch
                throw:{error, _, _} = R1 ->
                    api_result(Context, R1);
                throw:{error, _, _, _} = R2 ->
                    api_result(Context, R2);
                ?WITH_STACKTRACE(E, R, S)
                    lager:error("controller_api error: ~p:~p - ~p", [E, R, S]),
                    api_result(Context1, {error, internal_server_error, []})
            end
    end.

%% @doc Handle JSON request bodies.
-spec handle_json_request(#wm_reqdata{}, #context{}) -> {ok, #context{}} | {error, string()}.
handle_json_request(ReqData, Context) ->
    case wrq:get_req_header("content-type", ReqData) of
        "application/json" ++ _ ->
            decode_json_body(ReqData, Context);
        _ ->
            {ok, Context}
    end.

%% @doc Decode JSON request body.
-spec decode_json_body(#wm_reqdata{}, #context{}) -> {ok, #context{}} | {error, string()}.
decode_json_body(ReqData0, Context0) ->
    {ReqBody, ReqData} = wrq:req_body(ReqData0),
    Context = ?WM_REQ(ReqData, Context0),

    case ReqBody of
        <<>> -> {ok, Context};
        NonEmptyBody ->
            Json = try
                mochijson2:decode(NonEmptyBody)
            catch
                Type:Reason -> {error, {Type, Reason}}
            end,

            case Json of
                {error, Error} -> {error, Error};
                {struct, JsonStruct} ->
                    %% A JSON object: set key/value pairs in the context
                    Context1 = lists:foldl(
                        fun({Key, Value}, Context2) ->
                            z_context:set_q(z_convert:to_list(Key), Value, Context2)
                        end,
                        Context,
                        JsonStruct
                    ),
                    {ok, Context1};
                _ ->
                    %% A JSON list: don't alter context
                    {ok, Context}
            end
    end.

get_callback(Context) ->
    case z_context:get_q("callback", Context) of
        undefined ->
            case z_context:get_q("jsonp", Context) of
                undefined -> filter(z_context:get_q("jsoncallback", Context));
                Callback -> filter(Callback)
            end;
        Callback ->
            filter(Callback)
    end.

get_q_all(Context) ->
    proplists:delete("module",
    proplists:delete("method",
    proplists:delete("jsoncallback",
    proplists:delete("callback",
    proplists:delete("jsonp",
        z_context:get_q_all_noz(Context)))))).

filter(undefined) ->
    undefined;
filter(F) ->
    [ C || C <- F,      (C >= $0 andalso C =< $9)
                 orelse (C >= $a andalso C =< $z)
                 orelse (C >= $A andalso C =< $Z)
                 orelse C =:= $_
                 orelse C =:= $.
                 orelse C =:= $[
                 orelse C =:= $]
                 orelse C =:= $$
    ].
