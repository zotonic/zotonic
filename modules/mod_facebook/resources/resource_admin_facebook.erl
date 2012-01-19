%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2011 Maas-Maarten Zeeman
%% Date: 2011-08-09
%% @doc Page with all facebook settings.

%% Copyright 2011 Maas-Maarten Zeeman
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

-module(resource_admin_facebook).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-export([
    is_authorized/2,
    event/2
]).

-include_lib("resource_html.hrl").

is_authorized(ReqData, Context) ->
    z_acl:wm_is_authorized(use, mod_facebook, ReqData, Context).

html(Context) ->
    Vars = [
	    {page_admin_facebook, true}
	   ],
    Html = z_template:render("admin_facebook.tpl", Vars, Context),
    z_context:output(Html, Context).

event(#submit{message=admin_facebook}, Context) ->
    case z_acl:is_allowed(use, mod_facebook, Context) of
        true ->
            save_settings(z_context:get_q_all(Context), Context),
            z_render:growl("Saved the Facebook settings.", Context);
        false ->
            z_render:growl("You don't have permission to change the Facebook settings.", Context)
    end.

save_settings([], Context) ->
    Context;
save_settings([{Key, Value} | T], Context) when Key == "appid"; 
						Key == "appsecret" ; 
						Key == "scope"->
    m_config:set_value(mod_facebook, list_to_atom(Key), Value, Context),
    save_settings(T, Context);
save_settings([_|T], Context) ->
     save_settings(T, Context).

