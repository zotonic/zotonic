%% @doc Model for administrating WebSub subscriptions for import and export.
%% @copyright 2021 Marc Worrell
%% @author Marc Worrell <marc@worrell.nl>

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

-module(m_websub).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    update_export/6,
    delete_export/3,

    install/1
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").


%% @doc Update or insert a subscriber to a topic. The subscription is valid
%% for LeaseSecs seconds and can be deleted afterwards.
update_export(Callback, Topic, RscId, LeaseSecs, OptSecret, Context) ->
    ok.

%% @doc Delete a topic subscriber.
delete_export(Callback, Topic, Context) ->
    ok.



install(Context) ->
    % Publish request push of data from local to remote
    case z_db:table_exists(websub_export, Context) of
        true ->
            % id (serial)
            % is_push_needed (set if push should be done)
            % local_rsc_id (local)
            % remote_rsc_id (remote - if zotonic)
            % user_id (user authenticated when subscription was made)
            % callback_url
            % topic_url (topic used when subscribing, needed for WebSub unsubscribe)
            % secret
            % lease (datetime till valid)
            % handler (handler preparing outgoing data)
            % handler_state
            % type (mime type of data to export)
            % created
            % modified
            % pushed
            ok;
        false ->
            ok
    end,
    % Subscription import data from remote to local
    case z_db:table_exists(websub_import, Context) of
        true ->
            % id (serial)
            % is_import_needed (set if re-import requested)
            % local_rsc_id (local)
            % remote_rsc_id (remote - if zotonic)
            % user_id (user who added the subscription)
            % hub_url
            % topic_url
            % secret
            % lease (datetime till valid, need extension before)
            % handler (handler handling incoming data)
            % handler_state
            % type (mime type for http accept header)
            % created
            % modified
            % received
            ok;
        false ->
            ok
    end.
