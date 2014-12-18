%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009-2014 Arjan Scherpenisse, Driebit BV
%% @doc Use Twitter for logon and/or follow users on Twitter using the streaming HTTP API.
%%
%% Setup instructions:
%% * Enable the mod_twitter module
%% * Configure in the admin the twitter keys (Auth -> App Keys &amp; Authentication Services)
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
-behaviour(gen_server).

-mod_title("Twitter").
-mod_description("Use Twitter for logon, and/or follow users on Twitter using the streaming HTTP API.").
-mod_prio(401).
-mod_schema(1).
-mod_depends([admin, authentication]).
-mod_provides([twitter]).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
        event/2,
        manage_schema/2,
        observe_rsc_update_done/2,
        pid_observe_twitter_restart/3
    ]).

-export([
        follow_config/1,
        fetch_user_ids/2
    ]).

-include_lib("zotonic.hrl").

-record(state, {
            context, 
            twerl_pid=undefined,
            follow={[],[]}
        }).

-define(DELAY_RATELIMIT, 15*60*1000).
-define(DELAY_ERROR,      1*60*1000).
-define(DELAY_START,        10*1000).


event(#submit{message=admin_twitter}, Context) ->
    case z_acl:is_allowed(use, mod_admin_config, Context) of
        true ->
            save_settings(Context),
            z_notifier:notify(twitter_restart, Context),
            z_render:growl(?__("Saved the Twitter settings.", Context), Context);
        false ->
            z_render:growl(?__("You don't have permission to change the Twitter settings.", Context), Context)
    end.

save_settings(Context) ->
    lists:foreach(fun ({Key, Value}) ->
                        K1 = z_convert:to_list(Key),
                        case is_setting(K1) of
                            true -> m_config:set_value(mod_twitter, list_to_atom(K1), Value, Context);
                            false -> ok
                        end
                  end,
                  z_context:get_q_all_noz(Context)).

is_setting("consumer_key") -> true;
is_setting("consumer_secret") -> true;
is_setting("useauth") -> true;
is_setting("access_token") -> true;
is_setting("access_token_secret") -> true;
is_setting("follow") -> true;
is_setting(_) -> false.

observe_rsc_update_done(#rsc_update_done{id=Id}, Context) ->
    case m_rsc:p_no_acl(Id, twitter_id, Context) of
        Empty when Empty =:= <<>>; Empty =:= undefined; Empty =:= [] ->
            case m_identity:get_rsc(Id, twitter_id, Context) of
                undefined ->
                    ok;
                L when is_list(L) ->
                    m_identity:delete(proplists:get_value(id, L), Context),
                    z_notifier:notify(twitter_restart, Context)
            end;
        TwitterId ->
            case m_identity:get_rsc(Id, twitter_id, Context) of
                undefined ->
                    m_identity:insert(Id, twitter_id, TwitterId, Context),
                    z_notifier:notify(twitter_restart, Context);
                L ->
                    case proplists:get_value(key, L) of
                        TwitterId ->
                            ok;
                        _Changed ->
                            m_identity:delete(proplists:get_value(id, L), Context),
                            m_identity:insert(Id, twitter_id, TwitterId, Context),
                            z_notifier:notify(twitter_restart, Context)
                    end
            end
    end.

pid_observe_twitter_restart(Pid, twitter_restart, _Context) ->
    gen_server:cast(Pid, check_stream).

%%====================================================================
%% API
%%====================================================================
%% @spec start_link(Args) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.  The datamodel is installed before the server is started.
init(Args) ->
    process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),
    lager:md([
        {site, z_context:site(Context)},
        {module, ?MODULE}
      ]),
    timer:send_after(?DELAY_START, author_edges_upgrade),
    timer:send_after(?DELAY_START, check_stream),
    {ok, #state{
        context=z_context:new(Context),
        twerl_pid=undefined,
        follow={[],[]}
    }}.

handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

handle_cast(author_edges_upgrade, #state{context=Context} = State) ->
    handle_author_edges_upgrade(Context),
    {noreply, State};

handle_cast(check_stream, State) ->
    case check_stream(State) of
        {ok, State1} ->
            {noreply, State1};
        {{error, unauthorized}, State1} ->
            {noreply, State1};
        {{error, rate_limit}, State1} ->
            timer:send_after(?DELAY_RATELIMIT, check_stream),
            {noreply, State1};
        {{error, connection_limit}, State1} ->
            timer:send_after(?DELAY_RATELIMIT, check_stream),
            {noreply, State1};
        {{error, _}, State1} ->
            timer:send_after(?DELAY_ERROR, check_stream),
            {noreply, State1}
    end;

handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.


%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}

%% @doc Reconnect the follower process if it stopped.
handle_info({'EXIT', Pid, Reason}, #state{twerl_pid=Pid} = State) ->
    lager:warning("[~p] Twitter: received from twerl 'EXIT' ~p", 
                  [z_context:site(State#state.context), Reason]),
    timer:send_after(15000, ensure_started),
    {noreply, State#state{twerl_pid=undefined}};

handle_info(ensure_started, #state{twerl_pid=undefined} = State) ->
    handle_cast(check_stream, State);
handle_info(ensure_started, State) ->
    {noreply, State};

handle_info({tweet, JSON}, State) ->
    try
        handle_tweet(JSON, State#state.context)
    catch
        X:Y ->
            lager:error("[~p] Twitter: exception during tweet import ~p:~p, stacktrace: ~p", 
                        [z_context:site(State#state.context), X, Y, erlang:get_stacktrace()]),
            lager:info("[~p] Twitter: error tweet is: ~p", 
                        [z_context:site(State#state.context), JSON])
    end,
    {noreply, State};

handle_info(check_stream, State) ->
    handle_cast(check_stream, State);

handle_info(author_edges_upgrade, State) ->
    handle_cast(author_edges_upgrade, State);

handle_info(Info, State) ->
    lager:warning("[twitter] unknown info message ~p", [Info]),
    {noreply, State}.


%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, #state{twerl_pid=undefined}) ->
    ok;
terminate(_Reason, #state{twerl_pid=Pid}) ->
    twerl_stream_manager:stop(Pid),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

check_stream(#state{context=Context} = State) ->
    Consumer = oauth_twitter_client:get_consumer(Context),
    AccessToken = m_config:get_value(?MODULE, access_token, Context),
    AccessTokenSecret = m_config:get_value(?MODULE, access_token_secret, Context),
    case is_configured(Consumer, AccessToken, AccessTokenSecret) of
        true ->
            prepare_following(Consumer, AccessToken, AccessTokenSecret, State);
        false ->
            lager:info("[~p] Twitter: not configured for automatic tweet imports.", [z_context:site(Context)]),
            {ok, State}
    end.

is_configured(undefined, _AccessToken, _AccessTokenSecret) -> false;
is_configured(_Consumer, undefined, _AccessTokenSecret) -> false;
is_configured(_Consumer, <<>>, _AccessTokenSecret) -> false;
is_configured(_Consumer, _AccessToken, undefined) -> false;
is_configured(_Consumer, _AccessToken, <<>>) -> false;
is_configured(_Consumer, _AccessToken, _AccessTokenSecret) -> true.

prepare_following(Consumer, AccessToken, AccessTokenSecret, #state{context=Context, follow=Following} = State) ->
    %% Get list of twitter ids to follow
    FollowUsers = follow_users(Context),
    {FollowConfig, PhrasesConfig} = follow_config(Context),
    Follow = lists:usort(normalize_username(FollowUsers ++ FollowConfig)),
    Phrases = lists:usort(PhrasesConfig),
    case {{Follow,Phrases}, State#state.twerl_pid} of
        {{[],[]}, _Pid} ->
            lager:info("[~p] Twitter: nothing to follow", [z_context:site(Context)]),
            stop_stream(State),
            {ok, State};
        {Following, Pid} when is_pid(Pid) ->
            % Nothing changed
            {ok, State};
        _ ->
            lager:info("[~p] Twitter: following ~p users and ~p phrases", [z_context:site(Context), length(Follow), length(Phrases)]),
            State1 = ensure_twerl(State),
            stop_stream(State1),
            start_stream(Follow, Phrases, Consumer, AccessToken, AccessTokenSecret, State1)
    end.

stop_stream(#state{twerl_pid=undefined}) ->
    ok;
stop_stream(#state{twerl_pid=TwerlPid}) ->
    twerl_stream_manager:stop_stream(TwerlPid).

ensure_twerl(#state{twerl_pid=undefined} = State) ->
    {ok, Pid} = twerl_stream_manager:start_link(),
    Self = self(),
    twerl_stream_manager:set_callback(Pid, fun(Data) -> Self ! {tweet, Data} end),
    State#state{twerl_pid=Pid};
ensure_twerl(State) ->
    State.


start_stream(Follow, Phrases, Consumer, AccessToken, AccessTokenSecret, #state{twerl_pid=TwerlPid} = State) ->
    {ConsumerKey, ConsumerSecret, _SignMethod} = Consumer,
    case lookup_user_ids(Follow, State#state.context) of
        {ok, Follow1} ->
            FollowStr = iolist_to_binary(z_utils:combine(",", Follow1)),
            PhraseStr = iolist_to_binary(z_utils:combine(",", Phrases)),
            Params = [
                {"track", z_convert:to_list(PhraseStr)},
                {"follow", z_convert:to_list(FollowStr)}
            ],
            Params1 = lists:filter(fun({_,""}) -> false; (_) -> true end, Params),
            Auth = [
                z_convert:to_list(ConsumerKey),
                z_convert:to_list(ConsumerSecret),
                z_convert:to_list(AccessToken), 
                z_convert:to_list(AccessTokenSecret)
            ],
            twerl_stream_manager:set_endpoint(TwerlPid, {post, "https://stream.twitter.com/1.1/statuses/filter.json"}),
            twerl_stream_manager:set_auth(TwerlPid, {oauth, Auth}),
            twerl_stream_manager:set_params(TwerlPid, Params1),
            ok = twerl_stream_manager:start_stream(TwerlPid),
            {ok, State#state{follow={Follow,Phrases}}};
        {error, _} = Error ->
            lager:error("[~p] Twitter: error looking up user-ids for stream: ~p  (ids: ~p)", 
                        [z_context:site(State#state.context), Error, Follow]),
            {Error, State}
    end.

follow_users(Context) ->
    [ V || {V} <- z_db:q("SELECT key FROM identity WHERE type = 'twitter_id' LIMIT 4000", Context) ].

follow_config(Context) ->
    Config = m_config:get_value(?MODULE, follow, Context),
    Fs = [ z_string:trim(C) || C <- split(Config) ],
    Fs1 = [ F || F <- Fs, F =/= <<>> ],
    {Users,Phrases} = lists:partition(
                            fun
                                (<<"@", _/binary>>) -> true;
                                (_) -> false
                            end,
                            Fs1),
    {[normalize_username(U) || U <- Users], Phrases}.

normalize_username(<<"@", Username/binary>>) -> Username;
normalize_username(Username) -> Username.

split(undefined) ->
    [];
split(B) ->
    Bs = re:split(B, <<"[\n\r\f,]">>),
    [ z_string:trim(binary:replace(F, <<9>>, <<" ">>, [global])) || F <- Bs ].


%
% this is the tweet handler persumably you could do something useful here
%
handle_tweet(Tweet, Context) ->
    case lists:dropwhile(fun(Key) ->
                            not lists:keymember(Key, 1, Tweet)
                         end,
                         twitter_message_types())
    of
        [] ->
            lager:debug("[~p] Twitter: receive unknown data in stream: ~p", [z_context:site(Context), Tweet]);
        [Key|_] ->
            handle_tweet_type(Key, Tweet, Context)
    end.


%% @doc Different stream messages (from https://dev.twitter.com/docs/streaming-apis/messages)
twitter_message_types() ->
    [
        <<"limit">>,
        <<"event">>, 
        <<"delete">>, 
        <<"disconnect">>,
        <<"warning">>,
        <<"status_withheld">>,
        <<"user_withheld">>,
        <<"for_user">>,
        <<"friends">>,
        <<"scrub_geo">>,
        <<"control">>,
        <<"user">>
    ].


handle_tweet_type(<<"user">>, Tweet, Context) ->
    % AsyncContext = z_context:prune_for_async(Context),
    % spawn(fun() -> import_tweet(Tweet, AsyncContext) end);
    twitter_import_tweet:import_tweet(Tweet, Context);
handle_tweet_type(<<"delete">>, _Tweet, Context) ->
    % {"delete":{"status":{"user_id":42,"user_id_str":"42","id_str":"1234","id":1234}}}
    lager:debug("[~p] Twitter: streamer ignored delete request.", [z_context:site(Context)]);
handle_tweet_type(<<"limit">>, Tweet, Context) ->
    {Limit} = proplists:get_value(<<"limit">>, Tweet),
    lager:debug("[~p] Twitter: streamer received limit (~p).", 
               [z_context:site(Context), proplists:get_value(<<"track">>, Limit)]);
handle_tweet_type(<<"disconnect">>, Tweet, Context) ->
    {Disconnect} = proplists:get_value(<<"disconnect">>, Tweet),
    lager:warning("[~p] Twitter: streamer disconnect by Twitter because ~p, ~p", 
                  [z_context:site(Context),
                   proplists:get_value(<<"code">>, Disconnect),
                   proplists:get_value(<<"reason">>, Disconnect)]);
handle_tweet_type(<<"warning">>, Tweet, Context) ->
    {Warning} = proplists:get_value(<<"warning">>, Tweet),
    lager:warning("[~p] Twitter: streamer warning ~p, ~p", 
                  [z_context:site(Context),
                   proplists:get_value(<<"code">>, Warning),
                   proplists:get_value(<<"message">>, Warning)]);
handle_tweet_type(Key, _Tweet, Context) ->
    lager:debug("[~p] Twitter: streamer ignored ~p", [z_context:site(Context), Key]).


%%
%% @doc The datamodel that is used in this module, installed the first time the module is started.
%%
manage_schema(install, _Context) ->
    #datamodel{
        categories=[
            {tweet, text, [{title, <<"Tweet">>}]}
        ],
        resources=[
            {from_twitter, keyword, [{title, <<"From Twitter">>}]}
        ]
    }.


%% handle_author_edges_upgrade(Context)
%% @doc upgrade person->tweeted->tweet edges to tweed->author->person
handle_author_edges_upgrade(C) ->
    Context = z_acl:sudo(C),
    case m_rsc:name_to_id_cat(tweeted, predicate, Context) of
        {ok, Tweeted} ->
            lager:info("[~p] Twitter: Found old 'tweeted' predicate, upgrading...", [z_context:site(Context)]),
            Author = m_rsc:name_to_id_cat_check(author, predicate, Context),
            z_db:q("update edge set subject_id = object_id, object_id = subject_id, predicate_id = $1 where predicate_id = $2", 
                   [Author, Tweeted],
                   Context),
            m_rsc:delete(Tweeted, Context),
            ok;
        _ ->
            nop
    end.


%% Map all usernames to user-ids using https://dev.twitter.com/rest/reference/get/users/lookup
lookup_user_ids(Users, Context) ->
    {Ids,Names} = lists:partition(fun(U) -> z_utils:only_digits(U) end, Users),
    case fetch_user_ids(Names, Context) of
        {ok, Ids1} ->
            {ok, lists:usort(Ids++Ids1)};
        {error, _} = Error ->
            Error
    end.

fetch_user_ids(Names, Context) ->
    NamesArg = iolist_to_binary(z_utils:combine(",", Names)),
    Args = [
        {"screen_name", z_convert:to_list(NamesArg)}
    ],
    Access = {
        z_convert:to_list(m_config:get_value(?MODULE, access_token, Context)),
        z_convert:to_list(m_config:get_value(?MODULE, access_token_secret, Context))
    },
    case oauth_twitter_client:request(get, "users/lookup", Args, Access, Context) of
        {ok, Users} ->
            {ok, [ proplists:get_value(id_str, UserProps) || UserProps <- Users ]};
        {error, notfound} ->
            {ok, []};
        {error, _} = Error ->
            Error
    end.


