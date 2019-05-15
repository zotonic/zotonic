%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013 Marc Worrell
%% @doc Administrative interface version for use via a web site's frontend.

%% Copyright 2013 Marc Worrell
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

-module(mod_admin_frontend).

-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Admin Frontend").
-mod_description("Edit pages on a web site; subset of admin module for Bootstrap based web sites.").
-mod_depends([mod_admin, mod_mqtt]).
-mod_prio(500).

-export([
    event/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

event(#postback{message={admin_menu_edit, Args}}, Context) ->
    maybe_load_edit_panel(Args, Context);
event(#postback_notify{message= <<"admin-menu-edit">>}, Context) ->
    maybe_load_edit_panel([], Context);
event(#sort{} = S, Context) ->
    controller_admin_edit:event(S, Context).

maybe_load_edit_panel(Args, Context) ->
    case m_rsc:rid(z_context:get_q(<<"id">>, Context, proplists:get_value(id, Args)), Context) of
        undefined ->
            maybe_load_edit_cat(Args, Context);
        Id ->
            Vars = [
                {id, Id},
                {tree_id, m_rsc:rid(z_context:get_q(<<"tree_id">>, Context), Context)}
            ],
            Context1 = z_render:update("editcol", #render{template={cat, "_admin_frontend_edit.tpl"}, vars=Vars}, Context),
            z_render:add_script(<<"setTimeout(function() { z_editor_init(); }, 100);">>, Context1)
    end.

maybe_load_edit_cat(Args, Context) ->
    case m_rsc:rid(z_context:get_q(<<"cat">>, Context, proplists:get_value(cat, Args)), Context) of
        undefined ->
            Context;
        CatId ->
            Vars = [
                {id, undefined},
                {cat, CatId},
                {tree_id, m_rsc:rid(z_context:get_q(<<"tree_id">>, Context), Context)}
            ],
            Context1 = z_render:update("editcol", #render{template={cat, "_admin_frontend_edit.tpl", m_category:is_a(CatId, Context)}, vars=Vars}, Context),
            z_render:add_script(<<"setTimeout(function() { z_editor_init(); }, 100);">>, Context1)
    end.
