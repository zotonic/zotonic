%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Admin webmachine_resource.

%% Copyright 2009 Tim Benniks
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

-module(resource_admin_logon).
-author("Tim Benniks <tim@timbenniks.com>").


-export([
         allowed_methods/2,
         process_post/2,
         html/1
        ]).

-include_lib("resource_html.hrl").


allowed_methods(ReqData, Context) ->
    {['GET', 'POST'], ReqData, Context}.


html(Context) ->
    html(Context, []).

html(Context, Vars) ->
    RD1 = z_context:get_reqdata(Context),
    Redirect = case z_context:get_q("redirect", Context) of
        undefined -> wrq:raw_path(RD1);
		Redir -> Redir
	end,
	Vars1 = [
		{zp_username, z_context:get_q("zp-username", Context)},
		{redirect, Redirect} | Vars
	],
    Html = z_template:render("admin_logon.tpl", Vars1, Context),
    {Data, ContextOut} = z_context:output(Html, Context),
    RD2 = wrq:append_to_resp_body(Data, RD1),
    RD3 = wrq:set_resp_header("Content-Type", "text/html; charset=utf-8", RD2),
	ContextLogon = z_context:set_reqdata(RD3, ContextOut),
    {{halt, 401}, ContextLogon}.


process_post(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:ensure_all(Context1),
    Username = z_context:get_q("zp-username", Context2),
    Password = z_context:get_q("zp-password", Context2),

    case z_auth:logon_pw(Username, Password, Context2) of
        {true, ContextLogon} -> 
			Url = case z_context:get_q("redirect", ContextLogon, "") of
				"" ->  z_dispatcher:url_for(admin, ContextLogon);
				Redirect -> 
				    Me = lists:flatten(z_dispatcher:url_for(admin_logon, ContextLogon)),
				    case lists:prefix(Me, Redirect) of
				        true -> z_dispatcher:url_for(admin, ContextLogon);
				        false -> Redirect
				    end
			end,
			UrlAbs = z_context:abs_url(Url, ContextLogon),
			ReqData1 = z_context:get_reqdata(ContextLogon),
            {{halt, 302}, wrq:set_resp_header("Location", UrlAbs, ReqData1), ContextLogon};
        {_, ContextLogon} ->
			Vars = [
				{error, invalid_credentials},
				{redirect, z_context:get_q("redirect", ContextLogon, "")}
			],
            {Reply, ContextHtml} = html(ContextLogon, Vars),
            ?WM_REPLY(Reply, ContextHtml)
    end.



%% {% wire id=#logon type="submit" postback="logon" delegate="z_auth" %}
