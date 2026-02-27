%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2026 Marc Worrell
%% @doc Model for the zotonic site configuration
%% @end

%% Copyright 2009-2026 Marc Worrell
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
-moduledoc("
Retrieve information that is stored in the [site
configuration](/id/doc_developerguide_configuration_site_configuration#ref-site-configuration). If you want to query
values from the config table instead, you should use [m_config](/id/doc_model_model_config).

Note

In general the site configurarion is only accessible via the `m.site` template model for users with administrator
rights. Exceptions are keys starting with `public` or `{{ m.site.title }}`, hostname configurations and the *paglen*.



Fetch a site configuration key
------------------------------

Example, fetching the site configuration key \"hostname\":


```django
{{ m.site.hostname }}
```



Fetching all configurations
---------------------------

It is easy to loop over all site configurations:


```django
{% for key, value in m.site %}
  {{ key }} -- {{ value }} <br>
{% endfor %}
```



Overriding config values
------------------------

Zotonic has two places where a site's configuration is kept. One is in the site's config files, the other in the
config table. The config table (accessible through [m_config](/id/doc_model_model_config)) overrules any module
settings from the config file, for rows where the module key of the config value is set to site.

Within the site configuration, you can override module-specific configuration: Module configurations are defined with a
property key equal to the module name, with a property list as value. For example:


```erlang
{mod_foo, [ {hostname, \"localhost\"} ]}
```

will put the default \"hostname\" setting of the imaginary `mod_foo` module to `localhost`.



Default config values
---------------------

Sites have the following default config settings:

| Property                    | Description                                                                      | Example value       |
| --------------------------- | -------------------------------------------------------------------------------- | ------------------- |
| environment | Set the DTAP status of the site. Can be one of production, development, test, acceptance, education or backup. Default: production | `development` |
| hostname | The hostname of the site | `example.com` |
| title | The title of the site. | `\"My Awesome Blog\"` |
| protocol | The main protocol of the site. Used to construct urls. Default \"http\". | `\"https\"` |
| document_domain | The document domain used for cross domain iframe javascripts. Default is the same as the cookie_domain. | `www.example.com` |
| cookie_domain | The domain to use on cookies. This defaults to undefined, which will equal the domain of the current request. | `.example.com` |
| session_expire_inactive | User inactivity timeout after seeing that the user has not been active. Default: 14400 (4 hours) | `3600 (1 hour)` |
| autologon_expire | Auto logon cookie timeout setting. Default: 15552000 (3 months) | `31536000 (365 days)` |
| site | The name of the site, an atom. | `wwwzotonic.` |
| hsts | Indicate if the site should use Strict Transport Security. When set, the browser will no longer use insecure http to access the site. Warning: Be sure your site is accessible via https before enabeling this feature. Default: false | `true` |
| hsts_maxage               | The time, in seconds which browsers are allowed to remember the HSTS setting of the site. Default: 17280000 (200 days) |                     |
| hsts_include_subdomains | When set, the browser also does not use http for subdomains. Default: false      |                     |
| hsts_preload              | When set, the site's HSTS setting will be stored by the browser vender. This means that browsers only use https. Use with care, because it is hard to revert wrong settings. Default: false |                     |

Available Model API Paths
-------------------------

| Method | Path pattern | Description |
| --- | --- | --- |
| `get` | `/site/...` | Return the current site name (atom) from the request context. |
| `get` | `/environment/...` | Return the resolved DTAP environment (`development`, `test`, `acceptance`, `production`, `education`, or `backup`). |
| `get` | `/hostname/...` | Return the configured hostname for the current site/request. |
| `get` | `/hostname_port/...` | Return hostname including configured HTTP port. |
| `get` | `/hostname_ssl_port/...` | Return hostname including configured HTTPS port. |
| `get` | `/hostalias/...` | Return the configured `hostalias` site setting. |
| `get` | `/protocol/...` | Return the site protocol used for URL construction (typically `http` or `https`). |
| `get` | `/is_ssl/...` | Return whether the site is currently considered SSL-enabled. |
| `get` | `/title/...` | Return the configured site title as binary text. |
| `get` | `/subtitle/...` | Return the configured site subtitle as binary text (empty binary when unset). |
| `get` | `/email_from/...` | Return the effective sender address used for outgoing email. |
| `get` | `/pagelen/...` | Return default search page length (configured value or fallback default). |
| `get` | `/language/...` | Return the current request language. |
| `get` | `/default_language/...` | Return the site default language. |
| `get` | `/security/...` | Return generated `security.txt` data (`contact`, optional `policy`/`hiring`, and `expires`). |
| `get` | `/+key/...` | Return site config value for `+key` when the key is public or caller is admin; otherwise `undefined`. |
| `get` | `/` | Return all site configuration key/value pairs for admins, otherwise an empty list. No further lookups. |

`/+name` marks a variable path segment. A trailing `/...` means extra path segments are accepted for further lookups.
").
-author("Marc Worrell <marc@worrell.nl").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/3,
    environment/1,
    title/1,
    security/1,
    load_config/1,
    load_config/2,
    reload_config/1,
    all/1,
    get/2,
    get/3,
    put/4
]).

-include_lib("zotonic.hrl").

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"site">> | Rest ], _Msg, Context) ->
    {ok, {z_context:site(Context), Rest}};
m_get([ <<"environment">> | Rest ], _Msg, Context) ->
    {ok, {environment(Context), Rest}};
m_get([ <<"hostname">> | Rest ], _Msg, Context) ->
    {ok, {z_context:hostname(Context), Rest}};
m_get([ <<"hostname_port">> | Rest ], _Msg, Context) ->
    {ok, {z_context:hostname_port(Context), Rest}};
m_get([ <<"hostname_ssl_port">> | Rest ], _Msg, Context) ->
    {ok, {z_context:hostname_ssl_port(Context), Rest}};
m_get([ <<"hostalias">> | Rest ], _Msg, Context) ->
    {ok, {get(hostalias, Context), Rest}};
m_get([ <<"protocol">> | Rest ], _Msg, Context) ->
    {ok, {z_context:site_protocol(Context), Rest}};
m_get([ <<"is_ssl">> | Rest ], _Msg, Context) ->
    {ok, {z_context:is_ssl_site(Context), Rest}};
m_get([ <<"title">> | Rest ], _Msg, Context) ->
    {ok, {title(Context), Rest}};
m_get([ <<"subtitle">> | Rest ], _Msg, Context) ->
    SubTitle = case m_config:get_value(site, subtitle, Context) of
        undefined -> <<>>;
        T -> unicode:characters_to_binary(T)
    end,
    {ok, {SubTitle, Rest}};
m_get([ <<"email_from">> | Rest ], _Msg, Context) ->
    EmailFrom = z_email:get_email_from(Context),
    {ok, {EmailFrom, Rest}};
m_get([ <<"pagelen">> | Rest ], _Msg, Context) ->
    PageLen = case m_config:get_value(site, pagelen, Context) of
        undefined -> ?SEARCH_PAGELEN;
        <<>> -> ?SEARCH_PAGELEN;
        V -> z_convert:to_integer(V)
    end,
    {ok, {PageLen, Rest}};
m_get([ <<"language">> | Rest ], _Msg, Context) ->
    {ok, {z_context:language(Context), Rest}};
m_get([ <<"default_language">> | Rest ], _Msg, Context) ->
    {ok, {z_language:default_language(Context), Rest}};
m_get([ <<"security">> | Rest ], _Msg, Context) ->
    {ok, {security(Context), Rest}};
m_get([ Key | Rest ], _Msg, Context) when is_binary(Key) ->
    try
        KeyAtom = erlang:binary_to_existing_atom(Key, utf8),
        case m_config:is_public_config_key(Key) orelse z_acl:is_admin(Context) of
            true -> {ok, {get(KeyAtom, Context), Rest}};
            false -> {ok, {undefined, []}}
        end
    catch
        error:badarg ->
            {ok, {undefined, []}}
    end;
m_get([], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true -> {ok, {all(Context), []}};
        false -> {ok, {[], []}}
    end;
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.


%% @doc Return the current DTAP environment.
-spec environment( z:context() | atom() ) -> z:environment().
environment(Site) when is_atom(Site) ->
    environment(z_context:new(Site));
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

-spec title(z:context()) -> binary().
title(Context) ->
    case m_config:get_value(site, title, Context) of
        undefined ->
            <<>>;
        Title ->
            case unicode:characters_to_binary(Title) of
                T when is_binary(T) -> T;
                _ -> <<>>
            end
    end.

-spec load_config(atom()|z:context()) -> ok | {error, term()}.
load_config(#context{} = Context) ->
    load_config(z_context:site(Context));
load_config(Site) when is_atom(Site) ->
    case z_sites_manager:get_site_config(Site) of
        {ok, Config} ->
            load_config(Site, Config);
        {error, _} = Error -> Error
    end.

-spec load_config(atom()|z:context(), list()) -> ok.
load_config(#context{} = Context, Config) ->
    load_config(z_context:site(Context), Config);
load_config(Site, Config) when is_atom(Site) ->
    application:load(Site),
    lists:foreach(
        fun({K,V}) ->
            application:set_env(Site, K, V)
        end,
        Config).

-spec reload_config(SiteOrContext) -> ok | {error, Reason} when
    SiteOrContext :: atom() | z:context(),
    Reason :: term().
reload_config(#context{} = Context) ->
    reload_config(z_context:site(Context));
reload_config(Site) when is_atom(Site) ->
    case z_sites_manager:reload_site_config(Site) of
        ok ->
            case load_config(Site) of
                ok ->
                    ?LOG_INFO(#{
                        in => zotonic_core,
                        text => <<"Reloaded site configuration">>,
                        result => ok,
                        site => Site
                    }),
                    z_sites_dispatcher:update_dispatchinfo();
                {error, Reason} = Error ->
                    ?LOG_ERROR(#{
                        in => zotonic_core,
                        text => <<"Could not reload site configuration">>,
                        result => error,
                        reason => Reason,
                        site => Site
                    }),
                    Error
            end;
        {error, Reason} = Error ->
            ?LOG_ERROR(#{
                in => zotonic_core,
                text => <<"Could not reload site configuration">>,
                result => error,
                reason => Reason,
                site => Site
            }),
            Error
    end.

%% @doc Return the complete site configuration
-spec all(atom()|z:context()) -> list().
all(#context{} = Context) ->
    all(z_context:site(Context));
all(Site) when is_atom(Site) ->
    application:get_all_env(Site).

%% @doc Fetch a key from the site configuration
-spec get(atom(), z:context() | atom()) -> term() | undefined.
get(Key, Site) when is_atom(Site) ->
    get(Key, z_context:new(Site));
get(Key, Context) when is_atom(Key) ->
    try
        Site = z_context:site(Context),
        case application:get_env(Site, Key) of
            {ok, undefined} ->
                undefined;
            {ok, none} when Key =:= hostname ->
                case z_context:is_request(Context) of
                    true -> cowmachine_req:host(Context);
                    false -> undefined
                end;
            {ok, Value} ->
                Value;
            undefined ->
                undefined
        end
    catch
        error:badarg ->
            % Special case on site setup when the depcache is not yet running
            {ok, Cfg} = z_sites_manager:get_site_config(z_context:site(Context)),
            proplists:get_value(Key, Cfg)
    end.

%% @doc Fetch a nested key from the site configuration
-spec get(atom(), atom(), z:context()) -> term() | undefined.
get(site, Key, Context) when is_atom(Key) ->
    get(Key, Context);
get(Module, Key, Context) when is_atom(Key) ->
    case get(Module, Context) of
        undefined -> undefined;
        L when is_list(L) -> proplists:get_value(Key, L)
    end.

%% @doc Put the value in the site config (temporary, till restart)
-spec put(atom(), atom(), term(), z:context()) -> ok.
put(site, Key, Value, Context) ->
    application:set_env(z_context:site(Context), Key, Value);
put(Module, Key, Value, Context) ->
    L1 = case get(Module, Context) of
        undefined ->
            [ {Key, Value} ];
        L when is_list(L) ->
            [ {Key, Value} | proplists:delete(Key, L) ]
    end,
    application:set_env(z_context:site(Context), Module, L1).



%% @doc Return the security.txt configuration. See also https://securitytxt.org
-spec security(Context) -> SecurityConfig when
    Context :: z:context(),
    SecurityConfig :: #{ binary() => Value },
    Value :: binary() | #{ binary() => binary() }.
security(Context) ->
    ContactEmail = security_contact_email(Context),
    Contact1 = if
        ContactEmail =:= undefined -> #{};
        true -> #{ <<"email">> => ContactEmail }
    end,
    ContactUrl = security_contact_url(Context),
    Contact2 = if
        ContactUrl =:= undefined -> Contact1;
        true -> Contact1#{ <<"url">> => ContactUrl }
    end,
    Sec = #{
        <<"contact">> => Contact2
    },
    PolicyUrl = security_policy(Context),
    Sec1 = if
        PolicyUrl =:= undefined -> Sec;
        true -> Sec#{ <<"policy">> => PolicyUrl }
    end,
    HiringUrl = security_hiring(Context),
    Sec2 = if
        HiringUrl =:= undefined -> Sec1;
        true -> Sec1#{ <<"hiring">> => HiringUrl }
    end,
    Sec2#{
        <<"expires">> => security_expires(Context)
    }.

security_expires(Context) ->
    Expires = case m_config:get_value(site, security_expires, Context) of
        None when None =:= undefined; None =:= <<>> ->
            z_datetime:next_month(erlang:universaltime());
        Exp ->
            z_datetime:to_datetime(Exp)
    end,
    z_datetime:format_utc(Expires, "c", Context).


security_contact_email(Context) ->
    case m_config:get_value(site, security_email, Context) of
        None when None =:= undefined; None =:= <<>> ->
            case z_config:get(security_email) of
                undefined -> z_html:unescape(m_rsc:p(1, <<"mail_email">>, Context));
                E -> E
            end;
        E ->
            E
    end.

security_contact_url(Context) ->
    case m_config:get_value(site, security_url, Context) of
        None1 when None1 =:= undefined; None1 =:= <<>> ->
            case m_rsc:p(<<"page_security_contact">>, <<"page_url_abs">>, Context) of
                undefined -> z_config:get(security_url);
                P -> P
            end;
        P ->
            P
    end.

security_policy(Context) ->
    case m_config:get_value(site, security_policy_url, Context) of
        None when None =:= undefined; None =:= <<>> ->
            case m_rsc:p(<<"page_security_policy">>, <<"page_url_abs">>, Context) of
                undefined -> z_config:get(security_policy_url);
                P -> P
            end;
        P ->
            P
    end.

security_hiring(Context) ->
    case m_config:get_value(site, security_hiring_url, Context) of
        None when None =:= undefined; None =:= <<>> ->
            case m_rsc:p(<<"page_security_hiring">>, <<"page_url_abs">>, Context) of
                undefined -> z_config:get(security_hiring_url);
                P -> P
            end;
        P ->
            P
    end.
