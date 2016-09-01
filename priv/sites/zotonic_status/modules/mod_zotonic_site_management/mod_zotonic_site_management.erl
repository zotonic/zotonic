%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016 Marc Worrell
%% @doc Add a new site

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
-mod_description("Add and/or manage Zotonic sites.").
-mod_prio(500).

-include("zotonic.hrl").

-export([
    event/2
    ]).

event(#submit{message=addsite}, Context) ->
    true = z_auth:is_auth(Context),
    Sitename = z_context:get_q_validated(<<"sitename">>, Context),
    Hostname = z_context:get_q_validated(<<"hostname">>, Context),
    Title = z_context:get_q(<<"title">>, Context),
    DbDatabase = z_context:get_q_validated(<<"dbdatabase">>, Context),
    DbSchema = z_context:get_q(<<"dbschema">>, Context),
    DbHost = z_context:get_q(<<"dbhost">>, Context),
    DbPort = z_context:get_q(<<"dbport">>, Context),
    DbUser = z_context:get_q(<<"dbuser">>, Context),
    DbPassword = z_context:get_q(<<"dbpassword">>, Context),
    GitUrl = 
    Context.

