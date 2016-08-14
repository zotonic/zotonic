%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015 Marc Worrell
%% @doc Import tags from Instagram as media items
%% See: http://instagram.com/developer/authentication/

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

-module(instagram_import).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    poll/2,
    poll_next/3
]).

-include_lib("zotonic.hrl").

-define(MAX_PAGES, 10).

-spec poll(list(), #context{}) -> list().
poll([], _Context) ->
    [];
poll([{Tag, _Time}|Rest] = Ps, Context) ->
    case instagram_api:tagged(Tag, Context) of
        {ok, {NextUrl, Ds}} ->
            case import_items(Ds, Context) of
                {ok, N} when N > 0 -> poll_next(NextUrl, 1, Context);
                _ -> ok
            end,
            poll(Rest, Context);
        {error, _} = Error ->
            lager:warning("[instagram] Error during tag poll for tag ~p error ~p", [Tag, Error]),
            Ps
    end.

poll_next(undefined, _PageNr, _Context) ->
    ok;
poll_next(null, _PageNr, _Context) ->
    ok;
poll_next(<<>>, _PageNr, _Context) ->
    ok;
poll_next(_Url, PageNr, _Context) when PageNr > ?MAX_PAGES ->
    ok;
poll_next(Url, PageNr, Context) ->
    case instagram_api:tagged_nexturl(Url, Context) of
        {ok, {NextUrl, Ds}} ->
            case import_items(Ds, Context) of
                {ok, N} when N > 0 -> poll_next(NextUrl, PageNr+1, Context);
                _ -> ok
            end;
        {error, _} = Error ->
            lager:warning("[instagram] Error during url poll for ~p error ~p", [Url, Error]),
            Error
    end.


import_items(Ds, Context) ->
    Types = types(z_convert:to_bool(m_config:get_value(mod_instagram, import_photos, Context)),
                  z_convert:to_bool(m_config:get_value(mod_instagram, import_videos, Context))),
    import_items(Ds, Types, Context, 0).

import_items([], _Types, _Context, ImportCount) ->
    {ok, ImportCount};
import_items([D|Ds], Types, Context, ImportCount) ->
    MediaType = media_type(D),
    case lists:member(MediaType, Types) of
        true ->
            case import_item(import_rsc(MediaType, D, Context), Context) of
                ok ->
                    import_items(Ds, Types, Context, ImportCount+1);
                {error, _} ->
                    import_items(Ds, Types, Context, ImportCount)
            end;
        false ->
            import_items(Ds, Types, Context, ImportCount)
    end.

import_item(#import_resource{name=UniqueName} = Rsc, Context) ->
    case m_rsc:rid(UniqueName, Context) of
        undefined ->
            case z_notifier:first(Rsc, Context) of
                undefined ->
                    Props = [
                        {is_published, true}
                        | Rsc#import_resource.props
                    ],
                    case m_media:insert_url(
                                hd(Rsc#import_resource.media_urls),
                                Props,
                                z_acl:sudo(Context))
                    of
                        {ok, RscId} ->
                            lager:info("[instagram] imported item ~p url ~p", [RscId, Rsc#import_resource.source_url]),
                            ok;
                        {error, _} = Error ->
                            lager:info("[instagram] error during item import ~p: ~p", [Rsc#import_resource.source_url, Error]),
                            Error
                    end;
                _ ->
                    ok
            end;
        _RscId ->
            {error, duplicate}
    end.

media_type({struct, Ps}) ->
    case lists:keymember(<<"videos">>, 1, Ps) of
        false ->
            case lists:keymember(<<"images">>, 1, Ps) of
                true -> photo;
                false -> undefined
            end;
        true ->
            video
    end;
media_type(_) ->
    undefined.

import_rsc(photo, {struct, Ps}, Context) ->
    BaseRsc = import_rsc_base(Ps, Context),
    {struct, Images} = proplists:get_value(<<"images">>, Ps),
    {struct, Medium} = proplists:get_value(<<"standard_resolution">>, Images,
                                proplists:get_value(<<"low_resolution">>, Images)),
    BaseRsc#import_resource{
        props=[
            {category, image} | BaseRsc#import_resource.props
        ],
        media_urls = [
            proplists:get_value(<<"url">>, Medium)
        ]
    };
import_rsc(video, {struct, Ps}, Context) ->
    BaseRsc = import_rsc_base(Ps, Context),
    {struct, Videos} = proplists:get_value(<<"videos">>, Ps),
    {struct, Medium} = proplists:get_value(<<"standard_resolution">>, Videos,
                                proplists:get_value(<<"low_bandwidth">>, Videos)),
    BaseRsc#import_resource{
        props=[
            {category, video} | BaseRsc#import_resource.props
        ],
        media_urls = [
            proplists:get_value(<<"url">>, Medium)
        ]
    }.

import_rsc_base(Ps, Context) ->
    Id = proplists:get_value(<<"id">>, Ps),
    SourceUrl = proplists:get_value(<<"link">>, Ps),
    {struct, User} = proplists:get_value(<<"user">>, Ps),
    InsUserId = proplists:get_value(<<"id">>, User),
    UserId = find_user(InsUserId, Context),
    UserName = proplists:get_value(<<"username">>, User),
    % FullName = proplists:get_value(<<"full_name">>, User),
    UniqueName = <<"instagram_", (z_convert:to_binary(Id))/binary>>,
    {Long, Lat, LocDescr} = location(Ps),
    Caption = caption(Ps),
    #import_resource{
        source = instagram,
        source_id = Id,
        source_url = SourceUrl,
        source_user_id = InsUserId,
        user_id = UserId,
        name = UniqueName,
        props = [
            {title, iolist_to_binary([
                        UserName, ": ", z_string:truncate(Caption, 50)
                    ])},
            {short_title, LocDescr},
            {body, z_html:escape_link(Caption)},
            {location_lng, Long},
            {location_lat, Lat},
            {website, SourceUrl}
        ],
        media_urls = [],
        urls = [],
        data = Ps
    }.

location(Ps) ->
    case proplists:get_value(<<"location">>, Ps, null) of
        {struct, Loc} ->
            Lat = proplists:get_value(<<"latitude">>, Loc),
            Long = proplists:get_value(<<"longitude">>, Loc),
            Descr = proplists:get_value(<<"name">>, Loc),
            {Long, Lat, Descr};
        null ->
            {undefined, undefined, undefined}
    end.

caption(Ps) ->
    case proplists:get_value(<<"caption">>, Ps, null) of
        {struct, Caption} ->
            proplists:get_value(<<"text">>, Caption, <<>>);
        null ->
            <<>>
    end.


find_user(InsUserId, Context) ->
    case m_identity:lookup_by_type_and_key(instagram, InsUserId, Context) of
        undefined -> undefined;
        R -> proplists:get_value(rsc_id, R)
    end.

types(false, false) -> [];
types(true,  true)  -> [photo, video];
types(false, true)  -> [video];
types(true,  false) -> [photo].

