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
    ?WM_REPLY(true, Context1).


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
                         [] -> 
                             TheMod; %% method == module name
                         M2 -> M2
                     end;
                 M3 -> M3
             end,
    case z_utils:ensure_existing_module("service_" ++ TheMod ++ "_" ++ Method) of
        {ok, Module} ->
            Context2 = z_context:set(service_module, Module, Context1),
            try
                {z_service:http_methods(Module), ReqData, Context2}
            catch
                _X:_Y ->
                    {['GET', 'HEAD', 'POST'], ReqData, Context2}
            end;
        {error, not_found} ->
            %% The atom (service module) does not exist, return default methods.
            {['GET', 'HEAD', 'POST'], ReqData, Context1}
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
    Module = z_context:get(service_module, Context),
    api_result(ReqData, Context, Module:process_get(ReqData, Context)).


process_post(ReqData, Context) ->
    Module = z_context:get(service_module, Context),
    case Module:process_post(ReqData, Context) of
        ok ->
            {true, ReqData, Context};
        Result ->
            api_result(ReqData, Context, Result)
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
