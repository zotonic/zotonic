%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009-2014 Arjan Scherpenisse, Driebit BV
%% @doc Use Twitter for logon and/or follow users on Twitter using the streaming HTTP API.
%%
%% Setup instructions:
%% * Enable the mod_twitter module
%% * Configure in the admin the twitter keys (Auth -> External Services)
%% * Create a person in the Zotonic database, find a twitter ID on
%%   twitter, and put it in the person record on the admin edit page (sidebar)
%% * The module will start automatically to follow the users which have a twitter id set.

%% Copyright 2009 Arjan Scherpenisse
%% Copyright 2014 Driebit BV
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

-module(mod_twitter).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-mod_title("Twitter").
-mod_description("Use Twitter for logon, and/or import tweets from users or tags on Twitter.").
-mod_prio(401).
-mod_schema(2).
-mod_depends([admin, authentication]).
-mod_provides([twitter]).

%% interface functions
-export([
        init/1,
        event/2,
        manage_schema/2,
        observe_rsc_update_done/2,
        observe_tick_1h/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").


-define(DELAY_RATELIMIT, 15*60*1000).
-define(DELAY_ERROR,      1*60*1000).
-define(DELAY_START,        10*1000).

-define(STARTUP_DELAY_SECS, 30).
-define(POLL_TASK_NAME, <<"twitter-subscription-poller">>).


%% @doc Initialize or restart the poll task.
init(Context) ->
    z_pivot_rsc:insert_task_after(?STARTUP_DELAY_SECS, twitter_poller, poll, ?POLL_TASK_NAME, [], Context).

%% @doc Ensure our poller task is always there.
observe_tick_1h(tick_1h, Context) ->
    case z_pivot_rsc:get_task(twitter_poller, poll, ?POLL_TASK_NAME, Context) of
        undefined -> init(Context);
        _Task -> ok
    end.


event(#submit{message=admin_twitter}, Context) ->
    case z_acl:is_allowed(use, mod_admin_config, Context) of
        true ->
            save_settings(Context),
            m_twitter:update_config_subscriptions(Context),
            init(Context),
            z_render:growl(?__("Saved the Twitter settings.", Context), Context);
        false ->
            z_render:growl(?__("You don't have permission to change the Twitter settings.", Context), Context)
    end.

save_settings(Context) ->
    lists:foreach(
        fun ({Key, Value}) ->
            case is_setting(Key) of
                true -> m_config:set_value(mod_twitter, binary_to_atom(Key, 'utf8'), Value, Context);
                false -> ok
            end
        end,
        z_context:get_q_all_noz(Context)).

is_setting(<<"consumer_key">>) -> true;
is_setting(<<"consumer_secret">>) -> true;
is_setting(<<"useauth">>) -> true;
is_setting(<<"access_token">>) -> true;
is_setting(<<"access_token_secret">>) -> true;
is_setting(<<"follow">>) -> true;
is_setting(_) -> false.

-spec observe_rsc_update_done(#rsc_update_done{}, z:context()) -> ok.
observe_rsc_update_done(#rsc_update_done{ id = Id }, Context) ->
    TwId = m_rsc:p_no_acl(Id, twitter_id, Context),
    case m_twitter:normalize_key(TwId, user) of
        <<>> ->
            case m_identity:get_rsc(Id, twitter_id, Context) of
            undefined ->
                    ok;
                L when is_list(L) ->
                    m_identity:delete(proplists:get_value(id, L), Context)
            end;
        <<"@", _/binary>> = TwitterId ->
            save_identity(Id, TwitterId, Context);
        TwitterId ->
            save_identity(Id, <<"@", TwitterId/binary>>, Context)
    end.

save_identity(Id, TwitterId, Context) ->
    IsChanged = case m_identity:get_rsc(Id, twitter_id, Context) of
        undefined ->
            m_identity:insert(Id, twitter_id, TwitterId, Context),
            true;
        Idn ->
            case proplists:get_value(key, Idn) of
                TwitterId ->
                    false;
                _Changed ->
                    m_identity:delete(proplists:get_value(id, Idn), Context),
                    m_identity:insert(Id, twitter_id, TwitterId, Context),
                    true
            end
    end,
    m_twitter:update_identitiy_subscription(Id, Context),
    case IsChanged of
        true -> init(Context);
        false -> ok
    end.

%%====================================================================
%% Internal functions
%%====================================================================

% check_stream(#state{context=Context} = State) ->
%     Consumer = oauth_twitter_client:get_consumer(Context),
%     AccessToken = m_config:get_value(?MODULE, access_token, Context),
%     AccessTokenSecret = m_config:get_value(?MODULE, access_token_secret, Context),
%     case is_configured(Consumer, AccessToken, AccessTokenSecret) of
%         true ->
%             prepare_following(Consumer, AccessToken, AccessTokenSecret, State);
%         false ->
%             lager:info("Twitter: not configured for automatic tweet imports."),
%             {ok, State}
%     end.

% is_configured(undefined, _AccessToken, _AccessTokenSecret) -> false;
% is_configured(_Consumer, undefined, _AccessTokenSecret) -> false;
% is_configured(_Consumer, <<>>, _AccessTokenSecret) -> false;
% is_configured(_Consumer, _AccessToken, undefined) -> false;
% is_configured(_Consumer, _AccessToken, <<>>) -> false;
% is_configured(_Consumer, _AccessToken, _AccessTokenSecret) -> true.

%%
%% @doc The datamodel that is used in this module, installed the first time the module is started.
%%
manage_schema(Version, Context) ->
    ok = m_twitter:install(Version, Context),
    #datamodel{
        categories=[
            {tweet, text, [{title, <<"Tweet">>}]}
        ],
        resources=[
            {from_twitter, keyword, [{title, <<"From Twitter">>}]}
        ]
    }.


% %% Map all usernames to user-ids using https://dev.twitter.com/rest/reference/get/users/lookup
% lookup_user_ids(Users, Context) ->
%     {Ids,Names} = lists:partition(fun(U) -> z_utils:only_digits(U) end, Users),
%     case fetch_user_ids(Names, Context) of
%         {ok, Ids1} ->
%             {ok, lists:usort(Ids++Ids1)};
%         {error, _} = Error ->
%             Error
%     end.

% fetch_user_ids(Names, Context) ->
%     NamesArg = iolist_to_binary(z_utils:combine(",", Names)),
%     Args = [
%         {"screen_name", z_convert:to_list(NamesArg)}
%     ],
%     Access = {
%         z_convert:to_list(m_config:get_value(?MODULE, access_token, Context)),
%         z_convert:to_list(m_config:get_value(?MODULE, access_token_secret, Context))
%     },
%     case oauth_twitter_client:request(get, "users/lookup", Args, Access, Context) of
%         {ok, Users} ->
%             {ok, [ proplists:get_value(id_str, UserProps) || UserProps <- Users ]};
%         {error, notfound} ->
%             {ok, []};
%         {error, _} = Error ->
%             Error
%     end.
