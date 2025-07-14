%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011-2022 Marc Worrell <marc@worrell.nl>
%% @doc Default Zotonic site, used when no other site can handle the supplied Host.

%% Copyright 2011-2022 Marc Worrell
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

-module(zotonic_site_status).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Zotonic Status").
-mod_descr("Default Zotonic site, used when no other site can handle the supplied Host.").
-mod_prio(10).
-mod_depends([base, bootstrap]).
-mod_provides([acl]).

-export([
    init/1,

    sites_status_observer/3,

    observe_auth_validate/2,

    observe_user_is_enabled/2,
    observe_acl_logon/2,
    observe_acl_logoff/2,

    is_peer_allowed/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-spec init( z:context() ) -> ok.
init(Context) ->
    ok = zotonic_notifier:observe(sites_status, {?MODULE, sites_status_observer, [Context]}, self()).


-spec sites_status_observer( z:context(), list( {atom(), z_sites_manager:site_status()} ), term() ) -> ok.
sites_status_observer(Context, SitesStatus, _SitesManagerContext) ->
    z_mqtt:publish([ <<"model">>, <<"zotonic_status">>, <<"event">>, <<"sites_status">> ], SitesStatus, Context),
    ok.

%% @doc Check the username and password entered
observe_auth_validate( #auth_validate{ username = <<"wwwadmin">>, password = Password }, Context ) ->
    case is_peer_allowed(Context) of
        true ->
            case is_equal(Password, z_convert:to_binary(z_config:get(password))) of
                true ->
                    ?LOG_INFO(#{
                        text => <<"Zotonic status logon success from allowed IP address">>,
                        in => zotonic_site_status,
                        result => ok,
                        username => <<"wwwadmin">>,
                        ip => inet:ntoa(m_req:get(peer_ip, Context))
                    }),
                    {ok, 1};
                false ->
                    ?LOG_ERROR(#{
                        text => <<"Zotonic status logon failure from allowed IP address">>,
                        in => zotonic_site_status,
                        result => error,
                        reason => password,
                        ip => inet:ntoa(m_req:get(peer_ip, Context))

                    }),
                    {error, pw}
            end;
        false ->
            ?LOG_ERROR(#{
                text => <<"Zotonic status logon failure from non allowed IP address">>,
                in => zotonic_site_status,
                result => error,
                reason => blocked,
                ip => inet:ntoa(m_req:get(peer_ip, Context))
            }),
            {error, blocked}
    end;
observe_auth_validate( #auth_validate{ username = Username }, Context ) ->
    ?LOG_ERROR(#{
        text => <<"Zotonic status logon failure with non 'wwwadmin' username">>,
        in => zotonic_site_status,
        result => error,
        reason => pw,
        username => Username,
        ip => inet:ntoa(m_req:get(peer_ip, Context))
    }),
    {error, pw}.


%% @doc Check if a user is enabled.
observe_user_is_enabled(#user_is_enabled{ id = 1 }, _Context) -> true;
observe_user_is_enabled(#user_is_enabled{}, _Context) -> false.

%% @doc Let the user log on, this is the moment to start caching information.
observe_acl_logon(#acl_logon{ id = UserId }, Context) ->
    Context#context{user_id=UserId}.

%% @doc Let the user log off, clean up any cached information.
observe_acl_logoff(#acl_logoff{}, Context) ->
    Context#context{acl=undefined, user_id=undefined}.

%% @doc Check peer address to the system management IP allowlist
-spec is_peer_allowed( z:context() ) -> boolean().
is_peer_allowed(Context) ->
    Peer = m_req:get(peer_ip, Context),
    z_ip_address:ip_match(Peer, z_config:get(ip_allowlist_system_management)).

% Constant time comparison.
-spec is_equal(Extern :: binary(), Secret :: binary() ) -> boolean().
is_equal(A, B) -> is_equal(A, B, true).

is_equal(<<>>, <<>>, Eq) -> Eq;
is_equal(<<>>, _B, _Eq) -> false;
is_equal(<<_, A/binary>>, <<>>, _Eq) -> is_equal(A, <<>>, false);
is_equal(<<C, A/binary>>, <<C, B/binary>>, Eq) -> is_equal(A, B, Eq);
is_equal(<<_, A/binary>>, <<_, B/binary>>, _Eq) -> is_equal(A, B, false).

