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
    publish/2,
    subscribe/3,
    map_user_site/1,
    test/0]).

-include("zotonic.hrl").

init(_Opts) ->
    ok.

% @doc Usernames are in the form "user@sitename" or "user@foobar.com"
check(undefined, _) ->
    false;
check(_, undefined) ->
    false;
check(Username, Password) when is_binary(Username), is_binary(Password) ->
    case map_user_site(Username) of
        {ok, SiteUser, Context} ->
            case z_auth:logon_pw(SiteUser, Password, Context) of
                {false, _Context} ->
                    lager:info("MQTT logon failed for ~p on ~p", [SiteUser, z_context:site(Context)]),
                    false;
                UserContext ->
                    lager:debug("MQTT logon success for ~p on ~p", [SiteUser, z_context:site(UserContext)]),
                    {true, {zauth, z_acl:user(UserContext), z_context:site(UserContext)}}
            end;
        {error, _} -> 
            false
    end.

add(Username, Password) when is_binary(Username), is_binary(Password) ->
    {error, noacces}.

delete(Username) when is_binary(Username) ->
    {error, noacces}.


publish(Topic, {zauth, UserId, Host}) when is_integer(UserId) ->
    Context = z_acl:logon(UserId, z_context:new(Host)),
    case z_mqtt:publish(Topic, Context) of
        {error, _} = Error -> Error;
        _ -> ok
    end;
publish(_Topic, _Auth) ->
    {error, eacces}.

subscribe({Topic, Qos}, Pid, {zauth, UserId, Host}) when is_integer(UserId) ->
    Context = z_acl:logon(UserId, z_context:new(Host)),
    case z_mqtt:subscribe(Topic, Qos, Pid, Context) of
        {error, _} = Error -> Error;
        _ -> ok
    end;
subscribe(_TopicQos, _Pid, _Auth) ->
    {error, eacces}.


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
                                    lager:info("MQTT: no site found for ~p, using fallback", [Hostname]),
                                    fallback_site(Username)
                            end;
                        {'EXIT',_} ->
                            lager:info("MQTT: no site found for ~p, using fallback", [Hostname]),
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
