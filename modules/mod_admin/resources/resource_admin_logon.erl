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
    Html = z_template:render("admin_logon.tpl", Vars, Context),
    {Data, ContextOut} = z_context:output(Html, Context),
    RD1 = z_context:get_reqdata(ContextOut),
    RD2 = wrq:append_to_resp_body(Data, RD1),
    RD3 = wrq:set_resp_header("Content-Type", "text/html; charset=utf-8", RD2),
    {{halt, 401}, RD3, Context}.


process_post(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:ensure_all(Context1),
    Username = z_context:get_q("zp-username", Context2),
    Password = z_context:get_q("zp-password", Context2),

    case z_auth:logon_pw(Username, Password, Context2) of
        {true, ContextLogon} -> 
            Url = z_dispatcher:url_for(admin, ContextLogon),
            {{halt, 302}, wrq:set_resp_header("Location", Url, ReqData), ContextLogon};
        
        {_, ContextLogon} ->
            html(ContextLogon, [{error, invalid_credentials}])
    end.



%% {% wire id=#logon type="submit" postback="logon" delegate="z_auth" %}
