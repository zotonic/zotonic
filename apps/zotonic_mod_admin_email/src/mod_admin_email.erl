%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Marc Worrell
%% @doc Email configuration and testing in the admin.

%% Copyright 2021 Marc Worrell
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

-module(mod_admin_email).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Email config").
-mod_description("Email configuration and testing.").
-mod_prio(800).
-mod_depends([admin]).
-mod_provides([]).


%% interface functions
-export([
    event/2,
    observe_admin_menu/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").

event(#submit{ message = {test_email, []} }, Context) ->
    Email = z_string:trim( z_context:get_q(<<"email">>, Context) ),
    case z_context:get_q(<<"send">>, Context) of
        <<>> ->
            {ok, MsgNr} = z_email:send(#email{
                    to = Email,
                    html_tpl = "email_test.tpl",
                    vars = []
                }, Context),
            Context1 = z_render:update(
                "email_test_result",
                #render{
                    template = "_admin_email_test_result.tpl",
                    vars = [
                        {email, Email},
                        {msg_nr, MsgNr}
                    ]
                },
                Context),
            z_render:growl([
                ?__("Sent email to", Context), " ", z_html:escape(Email)
                ], Context1);
        undefined ->
            z_render:dialog(
                ?__("View email status", Context),
                "_dialog_email_status.tpl",
                [ {email, Email} ],
                Context)
    end.

observe_admin_menu(#admin_menu{}, Acc, Context) ->
    [
        #menu_item{id=admin_email,
                parent=admin_system,
                label=?__("Email configuration", Context),
                url={admin_email},
                visiblecheck={acl, use, mod_admin_config}}

        | Acc
    ].
