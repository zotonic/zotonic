%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013 Marc Worrell

%% @doc Acces control for MQTT topics

%% Copyright 2013 Marc Worrell
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

-module(z_mqtt_acl).

-export([
    is_allowed/3
    ]).

-include_lib("zotonic.hrl").
-include_lib("emqtt/include/emqtt.hrl").

-compile([{parse_transform, lager_transform}]).


%% TODO: first check ACL callbacks, then this local fallback

is_allowed(Action, Topic, #context{} = Context) when Action =:= subscribe; Action =:= publish ->
    LocalSite = z_convert:to_binary(z_context:site(Context)), 
    Parts = emqtt_topic:words(Topic),
    case z_acl:is_admin(Context) of
        true ->
            % Admin can do anything on any topic within the admin's site.
            case Parts of
                [<<"site">>, LocalSite | _] -> true;
                _ -> is_allowed_acl(Action, Topic, Parts, LocalSite, Context)
            end;
        false ->
            is_allowed_acl(Action, Topic, Parts, LocalSite, Context)
    end.


is_allowed_acl(Action, Topic, Words, LocalSite, Context) ->
    Object = #acl_mqtt{
        type=emqtt_topic:type(Words),   % wildcard | direct
        topic=Topic,
        words=Words,
        site=LocalSite,
        page_id=z_session_page:page_id(Context)
    },
    case z_acl:maybe_allowed(Action, Object, Context) of
        true -> true;
        false -> false;
        undefined -> is_allowed(Action, Topic, Words, LocalSite, Context)
    end.


is_allowed(_Action, _Topic, [<<"test">>], _Site, _Context) -> 
    true;
is_allowed(_Action, _Topic, [<<"site">>, Site, <<"test">>], Site, _Context) -> 
    true;

is_allowed(subscribe, _Topic, [<<"public">>], _Site, _Context) -> 
    true;
is_allowed(subscribe, _Topic, [<<"site">>, Site, <<"public">>], Site, _Context) ->
    true;

is_allowed(subscribe, _Topic, [<<"user">>], _Site, Context) -> 
    z_auth:is_auth(Context); 
is_allowed(subscribe, _Topic, [<<"site">>, Site, <<"user">>], Site, Context) -> 
    z_auth:is_auth(Context); 
is_allowed(_Action, _Topic, [<<"site">>, Site, <<"user">>, User], Site, Context) ->
    case z_convert:to_binary(z_acl:user(Context)) of
        User ->
            true;
        _ ->
            case m_identity:get_username(Context) of
                User -> true;
                _ -> false
            end 
    end;
is_allowed(subscribe, _Topic, [<<"site">>, Site, <<"page">>], Site, _Context) ->
    true;
is_allowed(publish, _Topic, [<<"site">>, Site, <<"rsc">>, RscId | _], Site, Context) ->
    z_acl:rsc_editable(z_convert:to_integer(RscId), Context);
is_allowed(subscribe, _Topic, [<<"site">>, Site, <<"rsc">>, RscId | _], Site, Context) ->
    z_acl:rsc_visible(z_convert:to_integer(RscId), Context);
is_allowed(Action, Topic, Words, Site, Context) ->
    is_allowed(Action, Topic, Words, Site, z_session_page:page_id(Context), Context).

is_allowed(_Action, _Topic, [<<"site">>, Site, <<"page">>, PageId], Site, PageId, _Context) ->
    true;
is_allowed(_Action, _Topic, _Words, _Site, _PageId, _Context) ->
    false.

