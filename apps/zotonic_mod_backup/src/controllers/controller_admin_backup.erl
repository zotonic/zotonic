%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2022 Marc Worrell
%% @doc Overview of all backups.

%% Copyright 2010-2022 Marc Worrell
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
    service_available/1,
    is_authorized/1,
    event/2,
    process/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").

service_available(Context) ->
    Context1 = z_context:set_noindex_header(Context),
    Context2 = z_context:set_nocache_headers(Context1),
    {true, Context2}.

is_authorized(Context) ->
    z_controller_helper:is_authorized([ {use, z_context:get(acl_module, Context, mod_backup)} ], Context).


process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    Config = case mod_backup:check_configuration() of
        {ok, Cfg} ->
            Cfg#{
                ok => true
            };
        {error, _} ->
            #{}
    end,
    Vars = [
        {is_filestore_enabled, mod_backup:is_filestore_enabled(Context)},
        {page_admin_backup, true},
        {backup_config, Config},
        {backup_in_progress, mod_backup:backup_in_progress(Context)}
    ],
	Html = z_template:render("admin_backup.tpl", Vars, Context),
	z_context:output(Html, Context).



event(#postback{message='config_encrypt_backups'}, Context) ->
    set_config(encrypt_backups, Context);
event(#postback{message='config_backup_panel'}, Context) ->
    set_config(admin_panel, Context);
event(#postback{message='config_backup_daily'}, Context) ->
    set_config(daily_dump, Context);
event(#submit{message={restore, Args}}, Context) ->
    {id, Id} = proplists:lookup(id, Args),
    case z_acl:rsc_editable(Id, Context) of
        true ->
            #upload{filename=Filename, tmpfile=Tmpfile} = z_context:get_q_validated(<<"file">>, Context),
            {ok, Data} = file:read_file(Tmpfile),
            Format = case filename:extension(Filename) of
                <<".json">> -> json;
                _ -> bert
            end,
            case catch z_notifier:first(#rsc_upload{id=Id, format=Format, data=Data}, Context) of
                {ok, NewId} ->
                    z_render:wire([{dialog_close, []},
                                   {redirect, [{dispatch, admin_edit_rsc}, {id,NewId}]}], Context);
                undefined ->
                    z_render:growl_error(?__("Sorry, there is no module that can import this.", Context), Context);
                {error, badarg} ->
                    z_render:growl_error(?__("Sorry, this is not a backup file or it is corrupted.", Context), Context);
                {error, Reason} ->
                    Message = error_message(Reason, Context),
                    z_render:growl_error(Message, Context);
                _Other ->
                    z_render:growl_error(?__("Sorry, there was an error replacing your page.", Context), Context)
            end;
        false ->
            z_render:growl_error(?__("Sorry, you don't have permission to change this page.", Context), Context)
    end.

error_message(duplicate_name, Context) ->
    ?__("There is already a page with this name.", Context);
error_message(eacces, Context) ->
    ?__("You don't have permission to create this page.", Context);
error_message(file_not_allowed, Context) ->
    ?__("You don't have the proper permissions to upload this type of file.", Context);
error_message(download_failed, Context) ->
    ?__("Failed to download the file.", Context);
error_message(infected, Context) ->
    ?__("This file is infected with a virus.", Context);
error_message(av_external_links, Context) ->
    ?__("This file contains links to other files or locations.", Context);
error_message(sizelimit, Context) ->
    ?__("This file is too large.", Context);
error_message(R, Context) ->
    ?LOG_WARNING(#{
        text => <<"Unknown page creation or upload error">>,
        in => zotonic_mod_backup,
        result => error,
        reason => R
    }),
    ?__("Error creating the page.", Context).


set_config(What, Context) ->
    case z_acl:is_admin_editable(Context) of
        true ->
            m_config:set_value(mod_backup, What, z_context:get_q(<<"triggervalue">>, Context), Context),
            z_render:growl(?__("Changed configuration.", Context), Context);
        false ->
            z_render:growl_error(?__("Sorry, you have no permission to change the configuration.", Context), Context)
    end.
