%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% Date: 2009-12-10
%% @doc Use Twitter for logon and/or follow users on Twitter using the streaming HTTP API.
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
-mod_description("Use Twitter for logon, and/or follow users on Twitter using the streaming HTTP API.").
-mod_prio(200).
-mod_schema(1).
-mod_depends([admin]).
-mod_provides([twitter]).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
         manage_schema/2,
         fetch/4, 
         observe_rsc_update_done/2,
         receive_chunk/2
]).

-include_lib("zotonic.hrl").

-record(state, {context, twitter_pid=undefined}).


observe_rsc_update_done(#rsc_update_done{id=Id}, Context) ->
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
            case Restart of
                true  -> 
                    z_notifier:notify(restart_twitter, Context);
                false -> ok
            end
    end.


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
    handle_author_edges_upgrade(Context),

    z_notifier:observe(restart_twitter, self(), Context),

    %% Start the twitter process
    case start_following(Context) of
        Pid when is_pid(Pid) ->
            {ok, #state{context=z_context:new(Context),twitter_pid=Pid}};
        undefined ->
            {ok, #state{context=z_context:new(Context)}};
        not_configured ->
            z_session_manager:broadcast(#broadcast{type="error", message="No configuration (mod_twitter.api_login / mod_twitter.api_password) found, not starting.", title="Twitter", stay=true}, z_acl:sudo(Context)),
            {ok, #state{context=z_context:new(Context)}}
    end.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

handle_cast({restart_twitter, _Context}, #state{context=Context,twitter_pid=Pid}=State) ->
    case Pid of
        undefined ->
            %% not running
            Pid2 = start_following(Context),
            {noreply, #state{context=Context,twitter_pid=Pid2}};
        _ ->
            %% Exit the process; will be started again.
            erlang:exit(Pid, restarting),
            {noreply, State#state{twitter_pid=undefined}}
    end;

handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.



%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
handle_info({'EXIT', _Pid, restarting}, #state{context=Context}=State) ->
    timer:sleep(500),
    Pid=start_following(Context),
    {noreply, State#state{twitter_pid=Pid}};

handle_info({'EXIT', _Pid, {error, _Reason}}, #state{context=Context}=State) ->
    timer:sleep(15000),
    Pid=start_following(Context),
    {noreply, State#state{twitter_pid=Pid}};

handle_info(_Info, State) ->
    {noreply, State}.


%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, State) ->
    z_notifier:observe(restart_twitter, self(), State#state.context),
    ok.


%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

start_following(Context) ->
    Login = case m_config:get_value(?MODULE, api_login, false, Context) of
                LB when is_binary(LB) ->
                    binary_to_list(LB);
                L -> L
            end,
    Pass  = case m_config:get_value(?MODULE, api_password, false, Context) of
                LP when is_binary(LP) ->
                    binary_to_list(LP);
                P -> P
            end,
    case Login of
        false ->
            error_logger:info_msg("No username/password configuration for mod_twitter. ~n"),
            not_configured;
        _ ->
            %% Get list of twitter ids to follow
            Follow1 = [binary_to_list(V) || {V} <- z_db:q("SELECT key FROM identity WHERE type = 'twitter_id' LIMIT 400", Context)],

            case Follow1 of
                [] ->
                    error_logger:info_msg("No follow configuration for mod_twitter. ~n"),
                    undefined;
                _ ->
                    URL = "https://" ++ Login ++ ":" ++ Pass ++ "@stream.twitter.com/1/statuses/filter.json",
                    Follow = z_utils:combine(",", Follow1),
                    Body = lists:flatten("follow=" ++ Follow),
                    z_session_manager:broadcast(#broadcast{type="notice", message="Now waiting for tweets to arrive...", title="Twitter", stay=false}, Context),
                    spawn_link(?MODULE, fetch, [URL, Body, 5, Context])
            end
    end.


%%
%% Main fetch process
%%
fetch(URL, Body, Sleep, Context) ->
    case httpc:request(post,
                      {URL, [], "application/x-www-form-urlencoded", Body},
                      [],
                      [{sync, false},
                       {stream, self}]) of
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
            {struct, Tweet} = mochijson:decode(Data),

            AsyncContext = z_context:prune_for_async(Context),
            F = fun() ->
                        {struct, User} = proplists:get_value("user", Tweet),
                        TweeterId = proplists:get_value("id", User),
                        case m_identity:lookup_by_type_and_key("twitter_id", TweeterId, AsyncContext) of
                            undefined ->
                                ?DEBUG("Unknown user..."),
                                z_session_manager:broadcast(#broadcast{type="error", message="Received a tweet for an unknown user.", title="Unknown user", stay=false}, Context);
                            Row ->
                                UserId = proplists:get_value(rsc_id, Row),
                                CategoryId = m_category:name_to_id_check(tweet, AsyncContext),
                                Props = [{title, proplists:get_value("screen_name", User) ++ " tweeted on " ++ proplists:get_value("created_at", Tweet)},
                                         {body, proplists:get_value("text", Tweet)},
                                         {source, proplists:get_value("source", Tweet)},
                                         {category_id, CategoryId},
                                         {tweet, Tweet},
                                         {is_published, true}],

                                AdminContext = z_acl:sudo(AsyncContext),
                                %% Create rsc
                                {ok, TweetId} = m_rsc:insert(Props, AdminContext),

                                %% Create edge
                                {ok, _} = m_edge:insert(TweetId, author, UserId, AdminContext),

                                %% Get images from the tweet and download them.
                                Urls = extract_urls(Tweet),
                                Ids = check_import_pictures(Urls, Context),
                                %% Create edges
                                [{ok, _} = m_edge:insert(TweetId, depiction, PictureId, Context) || PictureId <- Ids],

                                Message = proplists:get_value("screen_name", User) ++ ": " ++ proplists:get_value("text", Tweet),
                                z_session_manager:broadcast(#broadcast{type="notice", message=Message, title="New tweet!", stay=false}, AdminContext),
                                TweetId
                        end
                end,
            spawn(F);
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
            z_session_manager:broadcast(#broadcast{type="error", message="Twitter says the username/password is unauthorized.", title="Twitter module", stay=false}, z_acl:sudo(Context)),
            exit({error, {unauthorized, {Status, Headers}}});
        {http, {RequestId, Result}} ->
            exit({error, Result});

        %% start of streaming data
        {http,{RequestId, stream_start, Headers}} ->
            error_logger:info_msg("Streaming data start ~p ~n",[Headers]),
            ?MODULE:receive_chunk(RequestId, Context);

        %% streaming chunk of data
        %% this is where we will be looping around,
        %% we spawn this off to a seperate process as soon as we get the chunk and go back to receiving the tweets
        {http,{RequestId, stream, Data}} ->
            process_data(Data, Context),
            ?MODULE:receive_chunk(RequestId, Context);

        %% end of streaming data
        {http,{RequestId, stream_end, Headers}} ->
            error_logger:info_msg("Streaming data end ~p ~n", [Headers]),
            {ok, RequestId}

    after 120 * 1000 ->
            %% Timeout; respawn.
            exit({error, timeout})
    end.


%%
%% @doc The datamodel that is used in this module, installed the first time the module is started.
%%
manage_schema(install, _Context) ->
    #datamodel{categories=
               [
                {tweet,
                 text,
                 [{title, <<"Tweet">>}]}
               ],
               resources=
               [
                {from_twitter,
                 keyword,
                 [{title, <<"From Twitter">>}]}
               ]
              }.


%% handle_author_edges_upgrade(Context)
%% @doc upgrade person->tweeted->tweet edges to tweed->author->person
handle_author_edges_upgrade(C) ->
    Context = z_acl:sudo(C),
    case m_rsc:name_to_id_cat(tweeted, predicate, Context) of
        {ok, Tweeted} ->
            ?DEBUG("Found old 'tweeted' predicate, upgrading..."),
            Author = m_rsc:name_to_id_cat_check(author, predicate, Context),
            z_db:q("update edge set subject_id = object_id, object_id = subject_id, predicate_id = $1 where predicate_id = $2", [Author, Tweeted], Context),
            m_rsc:delete(Tweeted, Context),
            ok;
        _ ->
            nop
    end.


extract_urls(Tweet) ->
    {struct, Entitites} = proplists:get_value("entities", Tweet),
    {array, Urls} = proplists:get_value("urls", Entitites),
    [proplists:get_value("url", UO) || {struct, UO} <- Urls].
    


check_import_pictures([], _Context) ->
    [];
check_import_pictures(Urls, Context) ->
    %% Get oEmbed info on all Urls
    EmbedlyUrl = "http://api.embed.ly/1/oembed?urls=" ++ string:join([z_utils:url_encode(Url) || Url <- Urls], ","),
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
        httpc:request(EmbedlyUrl),
    {array, Pictures} = mochijson:decode(Body),

    Props = [P || {struct, P} <- Pictures],
    UrlProps = lists:zip(Urls, Props),
    %% Import pictures
    Ids = lists:filter(fun (X) -> not(z_utils:is_empty(X)) end, [import_oembed(Url, Props1, Context) || {Url, Props1} <- UrlProps]),

    %% Give 'em edges to the 'from twitter' keyword
    [{ok, _} = m_edge:insert(Id, subject, m_rsc:rid(from_twitter, Context), Context) || Id <- Ids],
    Ids.



%% @doc Import oEmbed-compatible proplist as a rsc.
%% @spec import_oembed(Url, Props, Context) -> undefined | int()
import_oembed(OriginalUrl, Props, Context) ->
    case oembed_category(proplists:get_value("type", Props)) of
        undefined ->
            undefined;
        Category ->
            RscProps = [{category, Category},
                        {title, proplists:get_value("title", Props)},
                        {summary, proplists:get_value("description", Props)},
                        {website, OriginalUrl},
                        {oembed, Props}],
            Url = proplists:get_value("url", Props),
            {ok, Id} = m_media:insert_url(Url, RscProps, Context),
            Id
    end.


%% @doc Mapping from oEmbed category to Zotonic category. undefined means: do not import.
oembed_category("photo") -> image;
oembed_category("image") -> image; %% not standard oEmbed, but returned by yfrog
oembed_category(_) -> undefined.



%% test() ->
%%     Tweet = [{"entities",
%%               {struct,
%%                [{"urls",
%%                  {array,
%%                   [{struct,
%%                     [{"indices",{array,[4,29]}},
%%                      {"url","http://twitpic.com/441ivo"},
%%                      {"expanded_url",null}]},
%%                    {struct,
%%                     [{"indices",{array,"$="}},
%%                      {"url","http://twitpic.com/4801nb"},
%%                      {"expanded_url",null}]}]}},
%%                 {"hashtags",{array,[]}},
%%                 {"user_mentions",{array,[]}}]}}],
%%     ["http://twitpic.com/441ivo", "http://twitpic.com/4801nb"] = extract_urls(Tweet),
%%     C = z_acl:sudo(z:c(scherpenisse)),
%%     check_import_pictures(extract_urls(Tweet), C).

