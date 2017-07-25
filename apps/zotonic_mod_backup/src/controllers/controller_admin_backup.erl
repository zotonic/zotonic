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

-module(controller_admin_backup).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    is_authorized/1,
    event/2
]).

-include_lib("zotonic_core/include/controller_html_helper.hrl").

is_authorized(Context) ->
    z_admin_controller_helper:is_authorized(mod_backup, Context).


html(Context) ->
    Vars = [
        {page_admin_backup, true},
        {backup_config, mod_backup:check_configuration()},
        {backup_in_progress, mod_backup:backup_in_progress(Context)}
    ],
	Html = z_template:render("admin_backup.tpl", Vars, Context),
	z_context:output(Html, Context).



event(#postback{message='config_backup_panel'}, Context) ->
    set_config(admin_panel, Context);
event(#postback{message='config_backup_daily'}, Context) ->
    set_config(daily_dump, Context);
event(#submit{message={restore, Args}}, Context) ->
    {id, Id} = proplists:lookup(id, Args),
    case z_acl:rsc_editable(Id, Context) of
        true ->
            #upload{filename=_Filename, tmpfile=Tmpfile} = z_context:get_q_validated(<<"file">>, Context),
            {ok, Data} = file:read_file(Tmpfile),
            case catch z_notifier:first(#rsc_upload{id=Id, format=bert, data=Data}, Context) of
                {ok, NewId} ->
                    z_render:wire([{dialog_close, []},
                                   {redirect, [{dispatch, admin_edit_rsc}, {id,NewId}]}], Context);
                undefined ->
                    z_render:growl_error(?__("Sorry, there is no mudule that can import this.", Context), Context);
                {error, badarg} ->
                    z_render:growl_error(?__("Sorry, this is not a backup file or it is corrupted.", Context), Context);
                _Other ->
                    z_render:growl_error(?__("Sorry, there was an error replacing your page.", Context), Context)
            end;
        false ->
            z_render:growl_error(?__("Sorry, you don't have permission to change this page.", Context), Context)
    end.

set_config(What, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            m_config:set_value(mod_backup, What, z_context:get_q(<<"triggervalue">>, Context), Context),
            z_render:growl(?__("Changed configuration.", Context), Context);
        false ->
            z_render:growl_error(?__("Sorry, you have no permission to change the configuration.", Context), Context)
    end.

