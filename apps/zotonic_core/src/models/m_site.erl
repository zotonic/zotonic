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
    load_config/1,
    load_config/2,
    all/1,
    get/2,
    get/3
]).

-include_lib("zotonic.hrl").

%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(hostname, #m{value=undefined}, Context) ->
    z_context:hostname(Context);
m_find_value(hostname_port, #m{value=undefined}, Context) ->
    z_context:hostname_port(Context);
m_find_value(hostname_ssl_port, #m{value=undefined}, Context) ->
    z_context:hostname_ssl_port(Context);
m_find_value(protocol, #m{value=undefined}, Context) ->
    z_context:site_protocol(Context);
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
-spec get(atom(), #context{}) -> term() | undefined.
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
            % Special case on site setup where the depcache is not yet running
            {ok, Cfg} = z_sites_manager:get_site_config(z_context:site(Context)),
            proplists:get_value(Key, Cfg)
    end.

%% @doc Fetch a nested key from the site configuration
-spec get(atom(), atom(), #context{}) -> term() | undefined.
get(site, Key, Context) when is_atom(Key) ->
    get(Key, Context);
get(Module, Key, Context) when is_atom(Key) ->
    case get(Module, Context) of
        undefined -> undefined;
        L when is_list(L) -> proplists:get_value(Key, L)
    end.
