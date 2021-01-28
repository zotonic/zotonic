%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2015 Marc Worrell
%% Date: 2009-08-07
%% @doc Allow editing and inserting config keys with string values.

%% Copyright 2009-2015 Marc Worrell
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

-module(mod_admin_config).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Admin config support").
-mod_description("Allow admins to edit the system configuration.").
-mod_prio(800).
-mod_depends([admin]).
-mod_provides([]).


%% interface functions
-export([
    observe_admin_menu/3,
    event/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").


observe_admin_menu(#admin_menu{}, Acc, Context) ->
    [
     #menu_item{id=admin_config_ssl,
                parent=admin_system,
                label=?__("SSL Certificates", Context),
                url={admin_config_ssl},
                visiblecheck={acl, use, mod_admin_config}},
     #menu_item{id=admin_config,
                parent=admin_system,
                label=?__("Config", Context),
                url={admin_config},
                visiblecheck={acl, use, mod_admin_config}}

     |Acc].

event(#submit{message={config_save, Args}}, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            Module = z_convert:to_atom( proplists:get_value(module, Args, undefined) ),
            Qs = z_context:get_q_all_noz(Context),
            lists:foreach(
                fun({Key, Value}) ->
                    {M, K} = split_key(Module, Key),
                    m_config:set_value(M, K, Value, Context)
                end,
                Qs),
            Context1 = z_render:growl("Saved the configuration.", Context),
            z_render:wire(proplists:get_all_values(on_success, Args), Context1);
        false ->
            z_render:growl_error("Only administrators can update configurations.", Context)
    end.

split_key(Module, Key) ->
    case binary:split(Key, <<".">>) of
        [ M, K ] ->
            {M, K};
        [ K ] ->
            {Module, K}
    end.
