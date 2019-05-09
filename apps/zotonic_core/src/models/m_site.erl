%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2017 Marc Worrell
%% @doc Model for the zotonic site configuration

%% Copyright 2009-2017 Marc Worrell
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

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/3,
    load_config/1,
    load_config/2,
    all/1,
    get/2,
    get/3,
    put/4
]).

-include_lib("zotonic.hrl").

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ hostname | Rest ], _Msg, Context) ->
    {ok, {z_context:hostname(Context), Rest}};
m_get([ hostname_port | Rest ], _Msg, Context) ->
    {ok, {z_context:hostname_port(Context), Rest}};
m_get([ hostname_ssl_port | Rest ], _Msg, Context) ->
    {ok, {z_context:hostname_ssl_port(Context), Rest}};
m_get([ hostalias | Rest ], _Msg, Context) ->
    {ok, {get(hostalias, Context), Rest}};
m_get([ protocol | Rest ], _Msg, Context) ->
    {ok, {z_context:site_protocol(Context), Rest}};
m_get([ is_ssl | Rest ], _Msg, Context) ->
    {ok, {z_context:is_ssl_site(Context), Rest}};
m_get([ title | Rest ], _Msg, Context) ->
    Title = m_config:get_value(site, title, Context),
    {ok, {Title, Rest}};
m_get([ subtitle | Rest ], _Msg, Context) ->
    SubTitle = m_config:get_value(site, subtitle, Context),
    {ok, {SubTitle, Rest}};
m_get([ pagelen | Rest ], _Msg, Context) ->
    PageLen = case m_config:get_value(site, pagelen, Context) of
        undefined -> ?SEARCH_PAGELEN;
        <<>> -> ?SEARCH_PAGELEN;
        V -> z_convert:to_integer(V)
    end,
    {ok, {PageLen, Rest}};
m_get([ Key | Rest ], _Msg, Context) when is_atom(Key) ->
    case z_acl:is_admin(Context) of
        true -> {ok, {get(Key, Context), Rest}};
        false -> {ok, {undefined, []}}
    end;
m_get([], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true -> {ok, {all(Context), []}};
        false -> {ok, {[], []}}
    end;
m_get(Vs, _Msg, _Context) ->
    lager:info("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {error, unknown_path}.


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

%% @doc Return the complete site configuration
-spec all(atom()|z:context()) -> list().
all(#context{} = Context) ->
    all(z_context:site(Context));
all(Site) when is_atom(Site) ->
    application:get_all_env(Site).

%% @doc Fetch a key from the site configuration
-spec get(atom(), z:context()) -> term() | undefined.
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

