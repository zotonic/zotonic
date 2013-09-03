%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013 Marc Worrell

%% @doc Authentication and access control module for emqtt

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

-module(emqtt_auth_zotonic).

-export([
    init/1,
    add/2,
    check/2,
    delete/1,
    is_allowed/3,
    map_user_site/1,
    test/0]).

-include("zotonic.hrl").

init(_Opts) ->
    ok.

% @doc Usernames are in the form "user@sitename" or "user@foobar.com"
check(undefined, _) -> false;
check(_, undefined) -> false;
check(Username, Password) when is_binary(Username), is_binary(Password) ->
    case map_user_site(Username) of
        {ok, SiteUser, Context} ->
            case z_auth:logon_pw(SiteUser, Password, Context) of
                {false, _Context} ->
                    false;
                UserContext ->
                    {ok, {zauth, z_acl:user(UserContext), z_context:site(UserContext)}}
            end;
        {error, _} -> 
            false
    end.

add(Username, Password) when is_binary(Username), is_binary(Password) ->
    {error, noacces}.

delete(Username) when is_binary(Username) ->
    {error, noacces}.


is_allowed(Action, Topic, {zauth, UserId, Host}) when is_integer(UserId) ->
    Context = z_acl:logon(UserId, z_context:new(Host)),
    is_allowed(Action, z_convert:to_binary(Topic), Context);
is_allowed(Action, Topic, #context{} = Context) when Action =:= subscribe; Action =:= publish ->
    LocalSite = z_convert:to_binary(z_context:site(Context)), 
    Parts = emqtt_topic:words(Topic),
    case z_acl:is_admin(Context) of
        true ->
            % Admin can do anything on any topic within the admin's site.
            case Parts of
                [<<"site">>, LocalSite | _] -> 
                    true;
                _ ->
                    is_allowed(Action, Topic, Parts, LocalSite, Context)
            end;
        false ->
            is_allowed(Action, Topic, Parts, LocalSite, Context)
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
is_allowed(subscribe, _Topic, [<<"site">>, Site, <<"page">>], Site, _Context) -> true;
is_allowed(Action, Topic, Words, Site, Context) ->
    is_allowed(Action, Topic, Words, Site, z_session_page:page_id(Context), Context).

is_allowed(_Action, _Topic, [<<"site">>, Site, <<"page">>, PageId], Site, PageId, _Context) ->
    true;
is_allowed(Action, Topic, Words, Site, PageId, Context) ->
    Object = #acl_mqtt{
        type=emqtt_topic:type(Words),   % wildcard | direct
        topic=Topic,
        words=Words,
        site=Site,
        page_id=PageId
    },
    z_acl:is_allowed(Action, Object, Context).


map_user_site(Username) when is_binary(Username) ->
    case binary:split(Username, <<"@">>, [global]) of
        [_] ->
            fallback_site(Username);
        Parts ->
            Hostname = lists:last(Parts),
            case z_sites_dispatcher:get_host_for_domain(Hostname) of
                undefined ->
                    case catch list_to_existing_atom(binary_to_list(Hostname)) of
                        Host when is_atom(Host) ->
                            case lists:member(Host, z_sites_manager:get_sites()) of
                                true ->
                                    {ok, local_username(Parts), z_context:new(Host)};
                                false ->
                                    fallback_site(Username)
                            end;
                        {'EXIT',_} -> 
                            fallback_site(Username)
                    end;
                {ok, Host} -> 
                    {ok, local_username(Parts), z_context:new(Host)}
            end
        end.

fallback_site(Username) ->
    {ok, Username, z_context:new(z_sites_dispatcher:get_fallback_site())}.

local_username(Parts) ->
    UserParts = lists:reverse(tl(lists:reverse(Parts))),
    iolist_to_binary(z_utils:combine($@, UserParts)). 



test() ->
    Fallback = z_sites_dispatcher:get_fallback_site(),
    {ok, <<"admin">>, #context{host=Fallback}} = map_user_site(<<"admin">>),
    {ok, <<"admin@foo.bar.not.exist">>, #context{host=Fallback}} = map_user_site(<<"admin@foo.bar.not.exist">>),
    ok.
