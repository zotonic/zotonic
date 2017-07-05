%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015 Marc Worrell
%%
%% @doc Instagram integration. Adds Instagram login and other functionalities.

%% Copyright 2015 Marc Worrell
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

-module(mod_instagram).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

-mod_title("Instagram").
-mod_description("Adds Instagram login and other Instagram related features.").
-mod_prio(500).

-export([
    event/2,
    poll/2,
    poll/3,
    check_subscription/1,
    observe_media_import/2
]).
-export([
    get_config/1
]).

-include_lib("zotonic_stdlib/include/z_url_metadata.hrl").

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% Don't poll for new tagged items more often than every 10 seconds
-define(POLL_DELAY, 10*1000).
-define(RETRY_DELAY, 600*1000).

%% Max API calls per hour, Instagram limit is 500, leave those 1000 for other calls.
-define(API_LIMIT, 4000).

-record(state, {
        site :: atom(),                             % Name of this site, for #context{}
        is_subscribed = false :: boolean(),
        last_poll = 0 :: pos_integer(),             % Last poll in seconds
        tags = [] :: list(),                        % All subscribed tags
        poll = [] :: list()                         % Tags to poll
    }).

-include_lib("zotonic_core/include/zotonic.hrl").


% You have to add your Instagram appid and secret to the config.
% By default, we only request access to the Instagram user's e-mail address.
-define(INSTAGRAM_SCOPE, "").


%% @doc Return the instagram appid, secret and scope
%% @spec get_config(Context) -> {AppId, Secret, Scope}
get_config(Context) ->
    { z_convert:to_list(m_config:get_value(mod_instagram, consumer_key, Context)),
      z_convert:to_list(m_config:get_value(mod_instagram, consumer_secret, Context)),
      z_convert:to_list(m_config:get_value(mod_instagram, scope, ?INSTAGRAM_SCOPE, Context))
    }.


event(#submit{message=admin_instagram}, Context) ->
    case z_acl:is_allowed(use, mod_admin_config, Context) of
        true ->
            save_settings(Context),
            z_render:growl(?__("Saved the Instagram settings.", Context), Context);
        false ->
            z_render:growl(?__("You don't have permission to change the Instagram settings.", Context), Context)
    end.

save_settings(Context) ->
    lists:foreach(fun ({Key, Value}) ->
                        case is_setting(Key) of
                            true -> m_config:set_value(mod_instagram, binary_to_atom(Key, 'utf8'), Value, Context);
                            false -> ok
                        end
                  end,
                  z_context:get_q_all_noz(Context)),
    check_subscription(Context).

is_setting(<<"consumer_key">>) -> true;
is_setting(<<"consumer_secret">>) -> true;
is_setting(<<"scope">>) -> true;
is_setting(<<"useauth">>) -> true;
is_setting(<<"follow">>) -> true;
is_setting(<<"access_token">>) -> true;
is_setting(<<"import_videos">>) -> true;
is_setting(<<"import_photos">>) -> true;
is_setting(_) -> false.


check_subscription(Context) ->
    gen_server:cast(name(Context), check_subscription).

poll(Tag, Context) when is_binary(Tag) ->
    poll(Tag, z_datetime:timestamp()-10, Context).

poll(Tag, Time, Context) when is_binary(Tag) ->
    gen_server:cast(name(Context), {poll, Time, Tag}).

name(Context) ->
    z_utils:name_for_site(?MODULE, Context).

%% @doc Recognize youtube and vimeo URLs, generate the correct embed code
observe_media_import(#media_import{host_rev=[<<"com">>, <<"instagram">> | _], metadata=MD} = _MI, Context) ->
    case z_url_metadata:p(<<"og:type">>, MD) of
        <<"instapp:photo">> -> media_import(MD, Context);
        <<"video">> -> media_import(MD, Context);
        _ -> undefined
    end;
observe_media_import(#media_import{}, _Context) ->
    undefined.

media_import(MD, Context) ->
    case z_url_metadata:p(image, MD) of
        undefined ->
            undefined;
        ImgUrl ->
            case decode_shared_data(MD#url_metadata.partial_data) of
                {ok, {struct, Props}} ->
                    try
                        media_import_shared_data(Props, MD, Context)
                    catch
                        ErrType:Err ->
                            Trace = erlang:get_stacktrace(),
                            lager:error("Error in instagram json decoding ~p:~p @ ~p",
                                        [ErrType, Err, Trace]),
                            media_import_md(ImgUrl, MD, Context)
                    end;
                _ ->
                    lager:error("Error in fetching & decoding the instagram shared data for ~s",
                                [z_url_metadata:p(url, MD)]),
                    media_import_md(ImgUrl, MD, Context)
            end
    end.

media_import_md(ImgUrl, MD, Context) ->
    #media_import_props{
        prio = 1,
        category = image,
        description = m_rsc:p_no_acl(image, title, Context),
        rsc_props = [
            {title, z_url_metadata:p(title, MD)},
            {summary, z_url_metadata:p(summary, MD)},
            {website, z_url_metadata:p(url, MD)}
        ],
        medium_props = [
            {mime, <<"image/jpeg">>}
        ],
        medium_url = ImgUrl
    }.

media_import_shared_data(Props, MD, Context) ->
    {struct, EntryData} = proplists:get_value(<<"entry_data">>, Props),
    PostPage = proplists:get_value(<<"PostPage">>, EntryData),
    Media = find_media(PostPage),
    {W,H} = media_dimensions(Media),
    Title = z_url_metadata:p(title, MD),
    Caption = proplists:get_value(<<"caption">>, Media),
    CaptionTruncated = z_string:truncate(Caption, 80),
    Summary = case CaptionTruncated of
        Caption -> Title;
        _ -> <<Caption/binary, " — "/utf8, Title/binary>>
    end,
    case proplists:get_value(<<"is_video">>, Media) of
        false ->
            ImgUrl = proplists:get_value(<<"display_src">>, Media),
            #media_import_props{
                prio = 1,
                category = image,
                description = m_rsc:p_no_acl(image, title, Context),
                rsc_props = [
                    {title, CaptionTruncated},
                    {summary, Summary},
                    {website, z_url_metadata:p(url, MD)}
                ],
                medium_props = [
                    {mime, <<"image/jpeg">>},
                    {width, W},
                    {height, H}
                ],
                medium_url = ImgUrl
            };
        true ->
            VideoUrl = proplists:get_value(<<"video_url">>, Media),
            PreviewImgUrl = proplists:get_value(<<"display_src">>, Media),
            #media_import_props{
                prio = 1,
                category = video,
                description = m_rsc:p_no_acl(video, title, Context),
                rsc_props = [
                    {title, CaptionTruncated},
                    {summary, Summary},
                    {website, z_url_metadata:p(url, MD)}
                ],
                medium_props = [
                    {mime, <<"video/mp4">>},
                    {width, W},
                    {height, H}
                ],
                medium_url = VideoUrl,
                preview_url = PreviewImgUrl
            }
    end.

media_dimensions(Media) ->
    {struct, Dim} = proplists:get_value(<<"dimensions">>, Media),
    Height = proplists:get_value(<<"height">>, Dim),
    Width = proplists:get_value(<<"width">>, Dim),
    {Width,Height}.

decode_shared_data(Data) ->
    case re:run(Data, "window._sharedData\s*=(.*);\s*</script>", [{capture,all_but_first,binary}]) of
        {match, [JSON]} -> {ok, mochijson2:decode(JSON)};
        nomatch -> {error, nojson}
    end.

find_media([{struct, List}|Rest]) ->
    case proplists:get_value(<<"media">>, List) of
        {struct, Media} -> Media;
        _ -> find_media(Rest)
    end.

%%====================================================================
%% API
%%====================================================================
%% @spec start_link(Args) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    gen_server:start_link({local, name(Context)}, ?MODULE, Args, []).

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
    Site = z_context:site(Context),
    lager:md([
        {site, Site},
        {module, ?MODULE}
      ]),
    timer:send_after(?POLL_DELAY, ensure_subscribed),
    timer:send_after(?POLL_DELAY, poll),
    {ok, #state{ site=Site }}.

handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

handle_cast(check_subscription, State) ->
    State1 = handle_check_subscription(State),
    {noreply, State1};

handle_cast({poll, Time, Tag}, #state{poll=Poll, tags=ImportTags} = State) ->
    case lists:member(Tag, ImportTags) of
        true ->
            lager:debug("[instagram] received poll for subscribed tag ~p", [Tag]),
            Poll1 = case lists:keymember(Tag, 1, Poll) of
                        false -> [{Tag,Time}|Poll];
                        true -> Poll
                    end,
            State1 = maybe_poll(State#state{poll=Poll1}),
            {noreply, State1};
        false ->
            lager:info("[instagram] dropped poll for unsubscribed tag ~p", [Tag]),
            {noreply, State}
    end;

handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.


%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}

handle_info(ensure_subscribed, State) ->
    State1 = handle_check_subscription(State),
    timer:send_after(?RETRY_DELAY, ensure_subscribed),
    {noreply, State1};

handle_info(poll, State) ->
    State1 = maybe_poll(State),
    timer:send_after(?POLL_DELAY, poll),
    {noreply, State1};

handle_info(Info, State) ->
    lager:warning("[instagram] unknown info message ~p", [Info]),
    {noreply, State}.


%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, _State) ->
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

maybe_poll(#state{is_subscribed=true, poll=Poll} = State) when Poll =/= [] ->
    case State#state.last_poll =< z_datetime:timestamp() - (?POLL_DELAY div 1000) of
        true ->
            Poll1 = instagram_import:poll(Poll, z_context:new(State#state.site)),
            State#state{last_poll=z_datetime:timestamp(), poll=Poll1};
        false ->
            State
    end;
maybe_poll(State) ->
    State.


handle_check_subscription(#state{site=Site} = State) ->
    Context = z_context:new(Site),
    Follow = get_follow(Context),
    Subs = get_subscription(Context),
    handle_check_subscription_1(Follow, Subs, Context, State).

handle_check_subscription_1(Tags, {ok, Subs}, Context, State) ->
    Tags1 = lists:usort(Tags),
    sync_subscription(Tags1, Subs, Context),
    State#state{is_subscribed=true, tags=Tags1};
handle_check_subscription_1(_Follow, _Subs, _Context, State) ->
    State#state{is_subscribed=false}.

%% TODO: stop on API error (esp. access token error)
sync_subscription(Tags, Subs, Context) ->
    lager:info("[instagram] subscribe to tags ~p", [Tags]),
    SubTags = [ T || {T,_} <- Subs ],
    New = Tags -- SubTags,
    Del = SubTags -- Tags,
    lists:foreach(fun(T) ->
                    {T, SubId} = proplists:lookup(T, Subs),
                    _ = instagram_api:unsubscribe(SubId, Context)
                  end,
                  Del),
    lists:foreach(fun(T) ->
                    _ = instagram_api:subscribe_tag(T, Context)
                  end,
                  New).

get_subscription(Context) ->
    case instagram_api:subscriptions(Context) of
        {ok, Subs} ->
            Tags = lists:flatten(
                        lists:map(
                            fun(Sub) ->
                                case proplists:get_value(object, Sub) of
                                    <<"tag">> ->
                                        {proplists:get_value(object_id, Sub),proplists:get_value(id, Sub)};
                                    _ ->
                                        []
                                end
                            end,
                            Subs)),
            {ok, Tags};
        {error, _} = Error ->
            Error
    end.

get_follow(Context) ->
    IsPhotoImport = z_convert:to_bool(m_config:get_value(mod_instagram, import_photos, Context)),
    IsVideoImport = z_convert:to_bool(m_config:get_value(mod_instagram, import_videos, Context)),
    case IsPhotoImport orelse IsVideoImport of
        false -> [];
        true -> tags(m_config:get_value(mod_instagram, follow, Context))
    end.

tags(undefined) ->
    [];
tags(<<>>) ->
    [];
tags(B) ->
    Bs = re:split(B, <<"[\t\n\r\f, ]">>),
    Bs1 = [ nohash(z_string:trim(F)) || F <- Bs ],
    [ z_string:to_lower(T) || T <- Bs1, T =/= <<>> ].

nohash(<<$#, Tag/binary>>) -> Tag;
nohash(Tag) -> Tag.
