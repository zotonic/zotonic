%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% @date 2009-12-10
%% @doc Follow users on Twitter using the streaming HTTP API.
%%
%% Setup instructions:
%% * Enable the mod_twitter module
%% * Put your login/password in the config keys mod_twitter.api_login
%%   and mod_twitter.api_password, respectively.
%% * Create a person in the Zotonic database, find a twitter ID on
%%   twitter, and put it in the person record on the admin edit page
%%   (sidebar)
%% * The module will start automatically to follow the users which have a twitter id set.

%% Copyright 2009 Arjan Scherpenisse
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
-mod_description("Follow persons from Zotonic on Twitter using the streaming API.").
-mod_prio(200).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
         fetch/4, event/2
]).

-include_lib("zotonic.hrl").

-record(state, {context, twitter_pid=undefined}).


%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),

    %% Manage our data model
    z_datamodel:manage(?MODULE, datamodel(), Context),

    %% Listen to resource-change events
    z_notifier:observe(rsc_update_done,   {?MODULE, event}, Context),

    %% Start the twitter process
    Pid = start_following(Context),

    {ok, #state{context=z_context:new(Context), twitter_pid=Pid}}.

%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%% @doc Trap unknown calls



handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

handle_cast(restart_http, State=#state{twitter_pid=Pid}) ->
    case Pid of
        undefined ->
            %% not running
            restart_http(State);
        _ ->
            %% Exit the process; will be started again.
            erlang:exit(Pid, restarting),
            {noreply, State}
    end;

handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.



%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
handle_info({'EXIT', Pid, restarting}, State=#state{twitter_pid=Pid}) ->
    ?DEBUG("Restarting"),
    timer:sleep(500),
    {noreply, restart_http(State)};

handle_info({'EXIT', Pid, {error, Reason}}, State=#state{twitter_pid=Pid}) ->
    ?DEBUG(Reason),
    timer:sleep(15000),
    {noreply, restart_http(State)};

handle_info(_Info, State) ->
    ?DEBUG(_Info),
    {noreply, State}.

restart_http(State=#state{context=Context}) ->
    NewPid = start_following(Context),
    State#state{twitter_pid=NewPid}.
    

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, State) ->
    z_notifier:detach(rsc_update_done, {?MODULE, event}, State#state.context),
    ok.


%% Handle z_notifier events
event({rsc_update_done, _Type, Id, _, _}, Context) ->
    case m_rsc:p(Id, twitter_id, Context) of
        undefined ->
            ok;
        TwitterId ->

            NonEmptyNewId = case TwitterId of
                                X when X =:= [] orelse X =:= <<>> orelse X =:= undefined -> false;
                                _ -> true
                            end,
            Restart = case m_identity:get_rsc(Id, twitter_id, Context) of
                          L when is_list(L) ->
                              case proplists:get_value(key, L) of
                                  TwitterId ->
                                      %% not changed
                                      false;
                                  _ ->
                                      m_identity:delete(proplists:get_value(id, L), Context),
                                      true
                              end;
                          _ -> NonEmptyNewId
                      end,
            case NonEmptyNewId of
                true -> m_identity:insert(Id, twitter_id, TwitterId, Context);
                _    -> ignore
            end,
            ?DEBUG("twitter"),
            ?DEBUG(Restart),
            ?DEBUG(NonEmptyNewId),
            case Restart of
                true  -> gen_server:cast(?MODULE, restart_http);
                false -> ok
            end
    end.


%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


start_following(Context) ->
    %% Get list of twitter ids to follow
    Follow1 = [binary_to_list(V) || {V} <- z_db:q("SELECT key FROM identity WHERE type = 'twitter_id' LIMIT 400", Context)],

    case Follow1 of
        [] ->
            error_logger:info_msg("No follow configuration for mod_twitter. ~n"),
            undefined;
        _ ->
            Login = m_config:get_value(?MODULE, api_login, false, Context),
            Pass  = m_config:get_value(?MODULE, api_password, false, Context),
            case Login of
                false ->
                    error_logger:info_msg("No username/password configuration for mod_twitter. ~n"),
                    undefined;
                _ ->
                    URL = "http://" ++ Login ++ ":" ++ Pass ++ "@stream.twitter.com/1/statuses/filter.json",
                    Follow = z_utils:combine(",", Follow1),
                    Body = lists:flatten("follow=" ++ Follow),
                    spawn_link(?MODULE, fetch, [URL, Body, 5, Context])
            end
    end.

%%
%% Main fetch process
%%
fetch(URL, Body, Sleep, Context) ->
    ?DEBUG("Started the Twitter follower."),
    case http:request(post,
                      {URL, [], "application/x-www-form-urlencoded", Body},
                      [],
                      [{sync, false},
                       {stream, self},
                       {verbose, trace}]) of
        {ok, RequestId} ->
            case receive_chunk(RequestId, Context) of
                {ok, _} ->
                                                % stream broke normally retry
                    timer:sleep(Sleep * 1000),
                    fetch(URL, Body, Sleep, Context);
                {error, timeout} ->
                    error_logger:info_msg("Timeout ~n"),
                    timer:sleep(Sleep * 1000),
                    fetch(URL, Body, Sleep, Context);
                {error, Reason} ->
                    error_logger:error_msg("Error ~p ~n", [Reason]),
                    timer:sleep(Sleep * 1000),
                    exit({error, Reason})
            end;
        _Reason ->
            error_logger:error_msg("Error ~p ~n", [_Reason]),
            timer:sleep(Sleep * 1000),
            fetch(URL, Body, Sleep, Context)
    end.

%
% this is the tweet handler persumably you could do something useful here
%
process_data(Data, Context) ->
    case Data of
        <<${, _/binary>> ->
            ?DEBUG("received tweet"),
            {struct, Tweet} = mochijson:decode(Data),

            AsyncContext = z_context:prune_for_async(Context),
            F = fun() ->
                        %%?DEBUG(Tweet),
                        {struct, User} = proplists:get_value("user", Tweet),
                        TweeterId = proplists:get_value("id", User),
                        case m_identity:lookup_by_type_and_key("twitter_id", TweeterId, AsyncContext) of
                            undefined ->
                                ?DEBUG("Unknown user...");
                            Row ->
                                UserId = proplists:get_value(rsc_id, Row),

                                GroupId = m_rsc:p(UserId, group_id, AsyncContext),

                                CategoryId = m_category:name_to_id_check(tweet, AsyncContext),
                                Props = [{title, proplists:get_value("screen_name", User) ++ " tweeted on " ++ proplists:get_value("created_at", Tweet)},
                                         {body, proplists:get_value("text", Tweet)},
                                         {source, proplists:get_value("source", Tweet)},
                                         {category_id, CategoryId},
                                         {tweet, Tweet},
                                         {is_published, true},
                                         {group_id, GroupId}],

                                AdminContext = z_acl:sudo(AsyncContext),
                                %% Create rsc
                                {ok, TweetId} = m_rsc:insert(Props, AdminContext),

                                %% Create edge
                                {ok, _} = m_edge:insert(UserId, tweeted, TweetId, AdminContext),

                                ?DEBUG("New tweet."),
                                TweetId
                        end
                end,
            spawn_link(F);
        _ ->
            ok
    end.


%%
%% Process a chunk of http data
%%
receive_chunk(RequestId, Context) ->
    receive
        {http, {RequestId, {error, Reason}}} when(Reason =:= etimedout) orelse(Reason =:= timeout) ->
            exit({error, timeout});
        {http, {RequestId, {{_, 401, _} = Status, Headers, _}}} ->
            exit({error, {unauthorized, {Status, Headers}}});
        {http, {RequestId, Result}} ->
            exit({error, Result});

        %% start of streaming data
        {http,{RequestId, stream_start, Headers}} ->
            error_logger:info_msg("Streaming data start ~p ~n",[Headers]),
            receive_chunk(RequestId, Context);

        %% streaming chunk of data
        %% this is where we will be looping around,
        %% we spawn this off to a seperate process as soon as we get the chunk and go back to receiving the tweets
        {http,{RequestId, stream, Data}} ->
            process_data(Data, Context),
            receive_chunk(RequestId, Context);

        %% end of streaming data
        {http,{RequestId, stream_end, Headers}} ->
            error_logger:info_msg("Streaming data end ~p ~n", [Headers]),
            {ok, RequestId}

    after 120 * 1000 ->
            %% Timeout; respawn.
            exit({error, timeout})
    end.



%%
%% The datamodel that is used in this module.
%%
datamodel() ->
    [{categories,
      [
       {tweet,
        text,
        [{title, <<"Tweet">>}]}
      ]
     },

     {predicates,
      [{tweeted,
        [{title, <<"Tweeted">>}],
        [{person, tweet}]
       }]
     }
    ].
