%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% Date: 2009-10-02
%% @doc OAuth.

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

-module(mod_oauth).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").
-behaviour(gen_server).

-mod_title("OAuth").
-mod_description("Provides authentication over OAuth.").
-mod_prio(900).


%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
         serve_oauth/3,
         request_is_signed/1,
         to_oauth_consumer/2,
         verify/6,
         str_value/2,
         test/0,
         authenticate/3,
         observe_service_authorize/2,
         observe_admin_menu/3
]).

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").


%%====================================================================
%% API
%%====================================================================
%% @spec start_link(Args) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),
    install_check(Context),
    {ok, []}.

%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.



%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, _State) ->
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

%%
%% Put this in a request to have it optionally served over OAuth.
%% Returns {true, NewContext} when succeeded, or {false, WebmachineResponse} when not.
%% Note that when the request is not signed, it will succeed as well, indicated with a 'none' atom.
%%
check_request_logon(ReqData, Context) ->
    % request is signed; verify it.
    case request_is_signed(Context) of
        false ->
            % Request was not signed.
            {none, Context};
        true ->
            case serve_oauth(ReqData, Context,
                fun(URL, Params, Consumer, Signature) ->
                        ParamToken = proplists:get_value("oauth_token", Params),
                        case m_oauth_app:secrets_for_verify(access, Consumer, ParamToken, Context) of
                            undefined ->
                                {false, authenticate("Access token not found.", ReqData, Context)};
                            Token ->
                                case m_oauth_app:check_nonce(Consumer, Token, str_value("oauth_timestamp", Params), str_value("oauth_nonce", Params), Context) of
                                    {false, Reason} ->
                                        {false, authenticate(Reason, ReqData, Context)};
                                    true ->
                                        case verify(ReqData, URL, Params, Consumer, Signature, Token) of
                                            true ->
                                                Context1 = case int_value(user_id, Token) of
                                                    undefined -> Context;
                                                    UID -> z_acl:logon(UID, Context)
                                                end,
                                                Context2 = z_context:set("oauth_consumer", Consumer, Context1),
                                                {true, Context2};
                                            false ->
                                                {false, authenticate("Signature verification failed.", ReqData, Context)}
                                        end
                                end
                        end
                end) of

                {{halt, Code}, ReqData2, Context2} ->
                    {false, {{halt, Code}, ReqData2, Context2}};

                Other -> Other
            end
    end.

verify(ReqData, URL, Params, Consumer, Signature, Token) ->
    SigMethod = str_value("oauth_signature_method", Params),
    oauth:verify(
        Signature,
        z_convert:to_list(wrq:method(ReqData)),
        URL,
        Params,
        to_oauth_consumer(Consumer, SigMethod),
        str_value(token_secret, Token)).

%%
%% This triggers OAuth authentication.
%%
request_is_signed(Context) ->
    case z_context:get_q("oauth_signature", Context) of
        undefined ->
            case z_context:get_req_header("authorization", Context) of
                "OAuth" ++ _ -> true;
                _ -> false
            end;
        _ ->
            true
    end.

is_base_string_param({"realm", _}) -> false;
is_base_string_param({"oauth_signature", _}) -> false;
is_base_string_param({"method", _}) -> false;
is_base_string_param({"module", _}) -> false;
is_base_string_param({"z_language", _}) -> false;
is_base_string_param(_) -> true.

%%
%% Transform a webmachine reqdata structure into the parameters that
%% are considered for OAuth signature verification.
%%
get_oauth_params(Context) ->
    Qs = z_context:get_q_all_noz(Context),
    All = case z_context:get_req_header("authorization", Context) of
        "OAuth" ++ _ = AuthHeader ->
            H = string:substr(AuthHeader, 7),
            oauth:header_params_decode(H) ++ Qs;
        _ ->
            Qs
    end,
    [ {z_convert:to_list(K), z_convert:to_list(V)} || {K,V} <- All ].

%%
%% Get an argument from either the request or the Authorization: header
%%

serve_oauth(ReqData, Context, Fun) ->
    Params = get_oauth_params(Context),
    case str_value("oauth_version", Params) of
        "1.0" ->
            ConsumerKey = str_value("oauth_consumer_key", Params),
            case m_oauth_app:consumer_lookup(ConsumerKey, Context) of
                undefined ->
                    authenticate("Consumer key not found.", ReqData, Context);
                Consumer ->
                    Signature = str_value("oauth_signature", Params),
                    BaseStringParams = lists:filter( fun is_base_string_param/1, Params ),
                    URL = z_convert:to_list(z_context:abs_url(wrq:path(ReqData), Context)),
                    Fun(URL, BaseStringParams, Consumer, Signature)
            end;
        Version ->
            authenticate("Unsupported OAuth version: " ++ Version ++ "\n", ReqData, Context)
    end.

%%
%% Helper functions
%%

str_value(Key, From) ->
    z_convert:to_list(proplists:get_value(Key, From)).

int_value(Key, From) ->
    z_convert:to_integer(proplists:get_value(Key, From)).


%% Convert a consumer record from the database representation to the presentation that erlang-oauth understands.

to_oauth_consumer(Consumer, "PLAINTEXT") ->
    {str_value(consumer_key, Consumer), str_value(consumer_secret, Consumer), plaintext};
to_oauth_consumer(Consumer, "HMAC-SHA1") ->
    {str_value(consumer_key, Consumer), str_value(consumer_secret, Consumer), hmac_sha1};
to_oauth_consumer(Consumer, "RSA-SHA1") ->
    {str_value(consumer_key, Consumer), str_value(consumer_secret, Consumer), rsa_sha1}.



%%
%% Send a WWW-Authenticate header
%%
authenticate(Reason, ReqData, Context) ->
    ReqData1 = wrq:set_resp_body(Reason ++ "\n", ReqData),
    ReqData2 = wrq:set_resp_header("WWW-Authenticate", "OAuth realm=\"\"", ReqData1),
    {{halt, 401}, ReqData2, Context}.


%% @doc Check is the shop module has been installed.  If not then install all db tables and rscs.
install_check(Context) ->
    case z_db:table_exists("oauth_application_registry", Context) of
        true ->
            ok;
        false ->
            oauth_install_data:install(Context)
    end.

test() ->
    Ctx  = z_context:new(default),
    ?DEBUG(m_oauth_app:consumer_lookup("Foo", Ctx)),
    ok.


%%
%% Whether consumer with this Id is allowed to execute Service.
%%
is_allowed(Id, Service, Context) ->
    not(z_service:needauth(Service)) orelse
        lists:member(Service, [proplists:get_value(service, S)
                               || S <- m_oauth_perms:all_services_for(Id, Context)]).


%% Main authorization hook, called from controller_api
observe_service_authorize(#service_authorize{service_module=Module}, Context) ->
    ReqData = z_context:get_reqdata(Context),
    case check_request_logon(ReqData, Context) of
        {none, Context} ->
            %% No OAuth; Authentication is required for this module...
            ServiceInfo = z_service:serviceinfo(Module, Context),
            authenticate(proplists:get_value(method, ServiceInfo) ++ ": " ++
                             z_service:title(Module) ++ "\n\nThis API call requires authentication.", ReqData, Context);

        {true, AuthorizedContext} ->
            %% OAuth succeeded; check whether we are allowed to exec this module
            ConsumerId = proplists:get_value(id, z_context:get("oauth_consumer", AuthorizedContext)),
            case is_allowed(ConsumerId, Module, AuthorizedContext) of
                true ->
                    {true, ReqData, AuthorizedContext};
                false ->
                    ReqData1 = wrq:set_resp_body("You are not authorized to execute this API call.\n", ReqData),
                    {{halt, 403}, ReqData1, AuthorizedContext}
            end;

        {false, Response} ->
            Response
    end.


observe_admin_menu(admin_menu, Acc, Context) ->
    [
     #menu_item{id=admin_oauth,
                parent=admin_auth,
                label=?__("API access", Context),
                url={admin_oauth},
                visiblecheck={acl, use, ?MODULE}}
     |Acc].
