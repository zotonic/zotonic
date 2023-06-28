%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-04-09
%%
%% @doc Model for the zotonic site configuration

%% Copyright 2009 Marc Worrell
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

-module(m_site).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,

    environment/1,
    security/1,
    all/1,
    get/2,
    get/3
]).

-include_lib("zotonic.hrl").

%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(hostname_no_port, #m{value=undefined}, Context) ->
    z_dispatcher:drop_port(get(hostname, Context));
m_find_value(document_domain, #m{value=undefined}, Context) ->
    z_context:document_domain(Context);
m_find_value(protocol, #m{value=undefined}, Context) ->
    z_context:site_protocol(Context);
m_find_value(environment, _M, Context) ->
    environment(Context);
m_find_value(security, _M, Context) ->
    security(Context);
m_find_value(Key, #m{value=undefined}, Context) ->
    get(Key, Context).

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> All
m_to_list(#m{value=undefined}, Context) ->
    all(Context).

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, Context) ->
    all(Context).


%% @doc Return the current DTAP environment
-spec environment( z:context() ) -> z:environment().
environment(Context) ->
    environment_atom( m_site:get(environment, Context) ).

environment_atom(development) -> development;
environment_atom(test) -> test;
environment_atom(acceptance) -> acceptance;
environment_atom(production) -> production;
environment_atom(education) -> education;
environment_atom(backup) -> backup;
environment_atom(undefined) -> z_config:get(environment);
environment_atom(<<>>) -> z_config:get(environment);
environment_atom("") -> z_config:get(environment);
environment_atom(L) when is_list(L) ->
    environment_atom( list_to_existing_atom(L) );
environment_atom(B) when is_binary(B) ->
    environment_atom( binary_to_existing_atom(B, utf8) ).

%% @doc Return the complete site configuration
all(Context) ->
    F = fun() ->
        {ok, Cfg} = z_sites_manager:get_site_config(Context#context.host),
        Cfg
    end,
    z_depcache:memo(F, site_config, Context).

%% @doc Fetch a key from the site configuration
get(Key, Context) when is_atom(Key) ->
    try
        case z_depcache:get(site_config, Key, Context) of
            {ok, undefined} ->
                undefined;
            {ok, none} when Key == hostname ->
                case z_context:is_request(Context) of
                    true -> sanitize_host(z_context:get_req_header("host", Context));
                    false -> undefined
                end;
            {ok, Cs} ->
                Cs;
            undefined ->
                All = all(Context),
                proplists:get_value(Key, All)
        end
    catch
        error:badarg ->
            % Special case on site setup where the depcache is not yet running
            {ok, Cfg} = z_sites_manager:get_site_config(Context#context.host),
            proplists:get_value(Key, Cfg)
    end.

sanitize_host(Host) ->
    sanitize_host(Host, []).

sanitize_host([], Acc) ->
    lists:reverse(Acc);
sanitize_host([C|Rest], Acc)
    when (C >= $a andalso C =< $z)
    orelse (C >= $A andalso C =< $Z)
    orelse (C >= $0 andalso C =< $9)
    orelse C =:= $-
    orelse C =:= $: ->
        sanitize_host(Rest, [C|Acc]);
sanitize_host(_, _Acc) ->
    [].


%% @doc Fetch a nested key from the site configuration
get(site, Key, Context) when is_atom(Key) ->
    get(Key, Context);
get(Module, Key, Context) when is_atom(Key) ->
    case get(Module, Context) of
        undefined -> undefined;
        L when is_list(L) -> proplists:get_value(Key, L)
    end.



%% @doc Return the security.txt configuration. See also https://securitytxt.org
-spec security(Context) -> SecurityConfig when
    Context :: z:context(),
    SecurityConfig :: proplists:proplist().
security(Context) ->
    ContactEmail = security_contact_email(Context),
    Contact1 = if
        ContactEmail =:= undefined -> [];
        true -> [ {email, ContactEmail} ]
    end,
    ContactUrl = security_contact_url(Context),
    Contact2 = if
        ContactUrl =:= undefined -> Contact1;
        true -> [ {url, ContactUrl} | Contact1 ]
    end,
    Sec = [ {contact, Contact2} ],
    PolicyUrl = security_policy(Context),
    Sec1 = if
        PolicyUrl =:= undefined -> Sec;
        true -> [ {policy, PolicyUrl} | Sec ]
    end,
    HiringUrl = security_hiring(Context),
    Sec2 = if
        HiringUrl =:= undefined -> Sec1;
        true -> [ {hiring, HiringUrl} | Sec1 ]
    end,
    [ {expires, security_expires(Context)} | Sec2 ].

security_expires(Context) ->
    Expires = case m_config:get_value(site, security_expires, Context) of
        None when None =:= undefined; None =:= <<>> ->
            z_datetime:next_month(erlang:universaltime());
        Exp ->
            z_datetime:to_datetime(Exp)
    end,
    erlydtl_dateformat:format_utc(Expires, "c", Context).


security_contact_email(Context) ->
    case m_config:get_value(site, security_email, Context) of
        None when None =:= undefined; None =:= <<>> ->
            z_config:get(security_email);
        E ->
            E
    end.

security_contact_url(Context) ->
    case m_config:get_value(site, security_url, Context) of
        None1 when None1 =:= undefined; None1 =:= <<>> ->
            case m_rsc:p(<<"page_security_contact">>, page_url_abs, Context) of
                undefined -> z_config:get(security_url);
                P -> P
            end;
        P ->
            P
    end.

security_policy(Context) ->
    case m_config:get_value(site, security_policy_url, Context) of
        None when None =:= undefined; None =:= <<>> ->
            case m_rsc:p(<<"page_security_policy">>, page_url_abs, Context) of
                undefined -> z_config:get(security_policy_url);
                P -> P
            end;
        P ->
            P
    end.

security_hiring(Context) ->
    case m_config:get_value(site, security_hiring_url, Context) of
        None when None =:= undefined; None =:= <<>> ->
            case m_rsc:p(<<"page_security_hiring">>, page_url_abs, Context) of
                undefined -> z_config:get(security_hiring_url);
                P -> P
            end;
        P ->
            P
    end.
