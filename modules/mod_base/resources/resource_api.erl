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

-module(resource_api).

-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([
    init/1,
    service_available/2,
    resource_exists/2,
    allowed_methods/2,
    process_post/2,
    is_authorized/2,
    content_types_provided/2,
    to_json/2
]).

-include_lib("webmachine_resource.hrl").
-include_lib("zotonic.hrl").

init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, Context),
    ?WM_REPLY(true, Context1).


allowed_methods(ReqData, Context) ->
    Context0 = ?WM_REQ(ReqData, Context),
    Context1 = z_context:ensure_qs(z_context:continue_session(Context0)),

    TheMod = case z_context:get_q("module", Context1) of
                 undefined -> z_convert:to_list(z_context:get(module, Context1));
                 M -> M
             end,

    Method = case z_context:get_q("method", Context1) of
                 undefined ->
                     case z_convert:to_list(z_context:get(method, Context1)) of
                         [] -> 
                             TheMod; %% method == module name
                         M2 -> M2
                     end;
                 M3 -> M3
             end,

    try
        {ok, Module}  = z_utils:ensure_existing_module("service_" ++ TheMod ++ "_" ++ Method),
        Context2 = z_context:set("module", Module, Context1),
        try
            {z_service:http_methods(Module), ReqData, Context2}
        catch
            _X:_Y ->
                {['GET', 'HEAD', 'POST'], ReqData, Context2}
        end
    catch
        error: badarg ->
            %% The atom (service module) does not exist, return default methods.
            {['GET', 'HEAD', 'POST'], ReqData, Context1}
    end.


%% TODO: refactor via z_notifier.
is_authorized(ReqData, Context) ->
    %% Check if we are authorized via a regular session.
    Context0 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:ensure_qs(z_context:continue_session(Context0)),
    
    case z_auth:is_auth(Context2) of
        true ->
            %% Yep; use these credentials.
            ?WM_REPLY(true, Context2);
        false ->
            %% No; see if we can use OAuth.
            Module = z_context:get("module", Context2),
            case mod_oauth:check_request_logon(ReqData, Context2) of
                {none, Context2} ->
                    case z_service:needauth(Module) of
                        false ->
                            %% No auth needed; so we're authorized.
                            {true, ReqData, Context2};
                        true ->
			    ServiceInfo = z_service:serviceinfo(Module, Context2),			    
                            %% Authentication is required for this module...
                            mod_oauth:authenticate(proplists:get_value(method, ServiceInfo) ++ ": " ++ z_service:title(Module) ++ "\n\nThis API call requires authentication.", ReqData, Context2)
                    end;

                {true, AuthorizedContext} ->
                    %% OAuth succeeded; check whether we are allowed to exec this module
                    ConsumerId = proplists:get_value(id, z_context:get("oauth_consumer", AuthorizedContext)),
                    case mod_oauth:is_allowed(ConsumerId, Module, AuthorizedContext) of
                        true ->
                            {true, ReqData, AuthorizedContext};
                        false ->
                            ReqData1 = wrq:set_resp_body("You are not authorized to execute this API call.\n", ReqData),
                            {{halt, 403}, ReqData1, AuthorizedContext}
                    end;

                {false, Response} ->
                    Response
            end
    end.


resource_exists(ReqData, Context) ->
    Exists = case z_context:get("module", Context) of
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


api_error(HttpCode, ErrCode, Message, ReqData, Context) ->
    R = {struct, [{error, {struct, [{code, ErrCode}, {message, Message}]}}]},
    {{halt, HttpCode}, wrq:set_resp_body(mochijson:encode(R), ReqData), Context}.


api_result(ReqData, Context, Result) ->
    case Result of
        {error, Err=missing_arg, Arg} ->
            api_error(400, Err, "Missing argument: " ++ Arg, ReqData, Context);

        {error, Err=unknown_arg, Arg} ->
            api_error(400, Err, "Unknown argument: " ++ Arg, ReqData, Context);

        {error, Err=syntax, Arg} ->
            api_error(400, Err, "Syntax error: " ++ Arg, ReqData, Context);

        {error, Err=not_exists, Arg} ->
            api_error(404, Err, "Resource does not exist: " ++ Arg, ReqData, Context);

        {error, Err=access_denied, _Arg} ->
            api_error(403, Err, "Access denied.", ReqData, Context);

        {error, Err, _Arg} ->
            api_error(500, Err, "Generic error.", ReqData, Context);

        Result2 ->
            try
                JSON = iolist_to_binary(mochijson:encode(Result2)),
                Body = case get_callback(Context) of
                           undefined -> JSON;
                           Callback -> [ Callback, $(, JSON, $), $; ]
                       end,
                {{halt, 200}, wrq:set_resp_body(Body, ReqData), Context}
            catch
                _E:plop ->
                    ReqData1 = wrq:set_resp_body("Internal JSON encoding error.\n", ReqData),
                    {{halt, 500}, ReqData1, Context}
            end
    end.


to_json(ReqData, Context) ->
    Module = z_context:get("module", Context),
    api_result(ReqData, Context, Module:process_get(ReqData, Context)).


process_post(ReqData, Context) ->
    Module = z_context:get("module", Context),
    case Module:process_post(ReqData, Context) of
        ok ->
            {true, ReqData, Context};
        Result ->
            api_result(ReqData, Context, Result)
    end.


get_callback(Context) ->
    case z_context:get_q("callback", Context) of
        undefined -> 
            filter(z_context:get_q("jsonp", Context));
        Callback ->
            filter(Callback)
    end.

filter(undefined) ->
    undefined;
filter(F) ->
    [ C || C <- F,      (C >= $0 andalso C =< $9) 
                 orelse (C >= $a andalso C =< $z)
                 orelse (C >= $A andalso C =< $Z)
    ].

