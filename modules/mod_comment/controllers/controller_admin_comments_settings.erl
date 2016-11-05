%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% Date: 2010-01-19
%% @doc Creates an editable overview of all categories.

%% Copyright 2010 Marc Worrell
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

-module(controller_admin_comments_settings).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    is_authorized/1,
    event/2
]).

-include_lib("controller_html_helper.hrl").

is_authorized(Context) ->
    z_admin_controller_helper:is_authorized(mod_comment, Context).


html(Context) ->
	Html = z_template:render("admin_comments_settings.tpl", [{page_admin_comments, true}], Context),
	z_context:output(Html, Context).

%% Copied from mod_seo and modified
event(#submit{message=admin_comments_settings}, Context) ->
    case z_acl:is_allowed(use, mod_comments_settings, Context) of
        true ->
            save_settings(z_context:get_q_all(Context), Context),
            z_render:growl("Saved the Comment Form settings.", Context);
        false ->
            z_render:growl("You don't have permission to change the Comment Form settings.", Context)
    end.


save_settings([], Context) -> Context;
save_settings([{<<"comments-moderate">>,  Value}|T], Context) ->
    set_config_value(moderate, Value, Context),
    save_settings(T, Context);
save_settings([{<<"comments-rating">>,  Value}|T], Context) ->
    set_config_value(rating, Value, Context),
    save_settings(T, Context);
save_settings([_|T], Context) -> 
    save_settings(T, Context).
    
    
set_config_value(Key, Value, Context) ->
    m_config:set_value(comments, Key, Value, Context),
    m_config:set_prop(comments, Key, no_config_edit, true, Context).