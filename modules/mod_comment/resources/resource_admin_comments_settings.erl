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

-module(resource_admin_comments_settings).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    is_authorized/2,
    event/2
]).

-include_lib("resource_html.hrl").

is_authorized(ReqData, Context) ->
    z_acl:wm_is_authorized(use, mod_comment, ReqData, Context).


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


save_settings([], Context) ->
    Context;
save_settings([{"comments" ++ _ = Key, Value} | T], Context) ->
    Value1 = clean(string:strip(Value, both), []),
    [Key1, Key2] = string:tokens(Key, "-"),
    m_config:set_value(list_to_atom(Key1), list_to_atom(Key2), Value1, Context),
    m_config:set_prop(list_to_atom(Key1), list_to_atom(Key2), no_config_edit, true, Context),
    save_settings(T, Context);
save_settings([_|T], Context) ->
    save_settings(T, Context).


clean([], Acc) ->
    lists:reverse(Acc);
clean([H|T], Acc) when
    H =:= 10 orelse H =:= 13 orelse H =:= $" orelse H =:= $' orelse
    H =:= $& orelse H =:= $< orelse H =:= $> ->
        clean(T, [32|Acc]);
clean([H|T], Acc) ->
    clean(T, [H|Acc]).
