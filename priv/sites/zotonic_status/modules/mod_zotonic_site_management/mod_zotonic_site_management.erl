%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016 Marc Worrell
%% @doc Site management module

%% Copyright 2016 Marc Worrell
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

-module(mod_zotonic_site_management).

-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Zotonic Site Management").
-mod_description("Manage Zotonic sites.").
-mod_prio(500).

-include("zotonic.hrl").

-export([
    event/2
    ]).

event(#submit{message=addsite}, Context) ->
    true = z_auth:is_auth(Context),
    Site = z_context:get_q_validated(<<"sitename">>, Context),
    Options = [
        {hostname, z_context:get_q_validated(<<"hostname">>, Context)},
        {skeleton, z_context:get_q_validated(<<"skel">>, Context)},
        {title, z_context:get_q(<<"title">>, Context)},
        {dbdatabase, z_context:get_q_validated(<<"dbdatabase">>, Context)},
        {dbschema, z_context:get_q_validated(<<"dbschema">>, Context)},
        {dbhost, z_context:get_q_validated(<<"dbhost">>, Context)},
        {dbport, z_context:get_q_validated(<<"dbport">>, Context)},
        {dbuser, z_context:get_q(<<"dbuser">>, Context)},
        {dbpassword, z_context:get_q(<<"dbpassword">>, Context)}
    ],
    case zotonic_status_addsite:addsite(Site, Options, Context) of
        ok ->
            %% .. Show success and the admin password
            Context;
        {error, Msg} when is_list(Msg); is_binary(Msg) ->
            notice(Site, Msg, Context)
    end.


% @doc Show a notice on the current webpage.
% show_notice(SiteName, Text, Context) ->
%     z_session_page:add_script(notice(SiteName, Text, Context)).

% @doc Render a notice.
notice(SiteName, Text, Context) ->
    Context1 = z_render:appear_top(
                        "notices",
                        #render{template="_notice.tpl", vars=[{site,SiteName},{notice,Text}]},
                        Context),
    z_render:wire({fade_out, [{selector, "#notices > p:gt(0)"}, {speed, 2000}]}, Context1).

