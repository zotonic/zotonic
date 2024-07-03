%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2023 Marc Worrell
%% @doc Page with all SEO settings.
%% @end

%% Copyright 2009-2023 Marc Worrell
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

-module(controller_admin_seo).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    is_authorized/1,
    process/4,
    event/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

is_authorized(Context) ->
    z_controller_helper:is_authorized([ {use, z_context:get(acl_module, Context, mod_seo)} ], Context).

process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    Vars = [
        {page_admin_seo, true}
    ],
	Html = z_template:render(<<"admin_seo.tpl">>, Vars, Context),
	z_context:output(Html, Context).


event(#submit{message=admin_seo}, Context) ->
    case z_acl:is_allowed(use, mod_seo, Context) of
        true ->
            save_settings(z_context:get_q_all(Context), Context),
            z_render:growl("Saved the SEO settings.", Context);
        false ->
            z_render:growl("You don't have permission to change the SEO settings.", Context)
    end.


save_settings([], Context) ->
    Context;
save_settings([{<<"seo", _/binary>> = Key, Value} | T], Context) ->
    Value1 = z_string:trim(Value),
    [Key1, Key2] = [binary_to_existing_atom(K, utf8) || K <- binary:split(Key, <<"-">>)],
    ok = m_config:set_value(Key1, Key2, Value1, Context),
    m_config:set_prop(Key1, Key2, no_config_edit, true, Context),
    save_settings(T, Context);
save_settings([_|T], Context) ->
    save_settings(T, Context).
