%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc Overview of all backups.

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

-module(resource_admin_backup).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    is_authorized/2
]).

-include_lib("resource_html.hrl").

is_authorized(ReqData, Context) ->
    z_acl:wm_is_authorized(use, mod_backup, ReqData, Context).


html(Context) ->
    Vars = [
        {page_admin_backup, true},
        {backups, mod_backup:list_backups(Context)},
        {backup_config, mod_backup:check_configuration(Context)},
        {backup_in_progress, mod_backup:backup_in_progress(Context)}
    ],
	Html = z_template:render("admin_backup.tpl", Vars, Context),
	z_context:output(Html, Context).
