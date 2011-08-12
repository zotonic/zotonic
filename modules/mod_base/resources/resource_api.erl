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
    Context1 = z_context:ensure_qs(Context0),
    %% 'ping' the service to ensure we loaded all the existing services.
    z_service:all(Context1),
    TheMod   = z_context:get_q("module", Context1),
    Method = case z_context:get(method_is_module, Context1) of
                 true -> TheMod;
                 _ -> z_context:get_q("method", Context1)
             end,
    try
        Module  = list_to_existing_atom("service_" ++ TheMod ++ "_" ++ Method),
        Context2 = z_context:set("module", Module, Context1),
        Context3 = z_context:set("partial_method", Method, Context2),
        try
            {z_service:http_methods(Module), ReqData, Context3}
        catch
            _X:_Y ->
                {['GET', 'HEAD', 'POST'], ReqData, Context3}
        end
    catch
        error: badarg ->
            %% Not exists
            {['GET', 'HEAD', 'POST'], ReqData, Context1}
    end.


is_authorized(ReqData, Context) ->
    %% Check if we are authorized via a regular session.
    Context2 = z_context:ensure_all(?WM_REQ(ReqData, Context)),
    case z_auth:is_auth(Context2) of
        true ->
            %% Yep; use these credentials.
            ?WM_REPLY(true, Context2);

        false ->
            %% No; see if we can use OAuth.
            Module = z_context:get("module", Context),
            case mod_oauth:check_request_logon(ReqData, Context) of
                {none, Context} ->
                    case z_service:needauth(Module) of
                        false ->
                            %% No auth needed; so we're authorized.
                            {true, ReqData, Context2};
                        true ->
                            %% Authentication is required for this module...
                            mod_oauth:authenticate(z_service:method(Module) ++ ": " ++ z_service:title(Module) ++ "\n\nThis API call requires authentication.", ReqData, Context2)
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
    Module = z_context:get("module", Context),
    {lists:member(Module, z_service:all(Context)), ReqData, Context}.


content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json},
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
                {{halt, 200}, wrq:set_resp_body(mochijson:encode(Result2), ReqData), Context}
            catch
                _E: R ->
                    ?DEBUG(R),
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
