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
    to_json/2,
    get_q_all/1
]).

-include_lib("controller_webmachine_helper.hrl").
-include_lib("zotonic.hrl").

init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, Context),
    z_context:lager_md(Context1),
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
                            api_error(500, 0, "No service authorization method available", ReqData, Context2);
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

api_error(HttpCode, ErrCode, Message, ReqData, Context) ->
    R = {struct, [{error, {struct, [{code, ErrCode}, {message, Message}]}}]},
    {{halt, HttpCode}, wrq:set_resp_body(mochijson:encode(R), ReqData), Context}.


api_result(Context, Result) ->
    ReqData = z_context:get_reqdata(Context),
    case Result of
        {error, Err=missing_arg, Arg} ->
            api_error(400, Err, "Missing argument: " ++ Arg, ReqData, Context);

        {error, Err=unknown_arg, Arg} ->
            api_error(400, Err, "Unknown argument: " ++ Arg, ReqData, Context);

        {error, Err=syntax, Arg} ->
            api_error(400, Err, "Syntax error: " ++ Arg, ReqData, Context);

        {error, Err=unauthorized, _Arg} ->
            api_error(401, Err, "Unauthorized.", ReqData, Context);
        
        {error, Err=not_exists, Arg} ->
            api_error(404, Err, "Resource does not exist: " ++ Arg, ReqData, Context);

        {error, Err=access_denied, _Arg} ->
            api_error(403, Err, "Access denied.", ReqData, Context);

        {error, Err, _Arg} ->
            api_error(500, Err, "Generic error.", ReqData, Context);

        Result2 ->
            try
                JSON = result_to_json(Result2),
                Body = case get_callback(Context) of
                           undefined -> JSON;
                           Callback -> [ Callback, $(, JSON, $), $; ]
                       end,
                {{halt, 200}, wrq:set_resp_body(Body, ReqData), Context}
            catch
                E:R ->
                    lager:warning("API error: ~p:~p", [E,R]),
                    ReqData1 = wrq:set_resp_body("Internal JSON encoding error.\n", ReqData),
                    {{halt, 500}, ReqData1, Context}
            end
    end.

    result_to_json(B) when is_binary(B) -> B;
    result_to_json({binary_json, R}) -> iolist_to_binary(mochijson:binary_encode(R));
    result_to_json(R) -> iolist_to_binary(mochijson:encode(R)).
    

to_json(ReqData, Context) ->
    Context0 = ?WM_REQ(ReqData, Context),
    Module = z_context:get(service_module, Context0),
    {Context1, Result} = 
        case Module:process_get(ReqData, Context0) of
            {R, C=#context{}} -> {C, R};
            R -> {Context0, R}
        end,
    api_result(Context1, Result).


process_post(ReqData, Context0) ->
    Context = ?WM_REQ(ReqData, Context0),
    case handle_json_request(ReqData, Context) of
        {error, _Reason} ->
            api_result(Context, {error, syntax, "invalid JSON in request body"});
        {ok, Context1} ->
            Module = z_context:get(service_module, Context1),
            ReqData1 = z_context:get_reqdata(Context1),
            case Module:process_post(ReqData1, Context1) of
                ok ->
                    {true, ReqData1, Context1};
                {Result, Context2=#context{}} ->
                    api_result(Context2, Result);
                Result ->
                    api_result(Context1, Result)
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
