%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-09
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
    all/1,
    get/2,
    get/3,
    get_all/2
]).

-include_lib("zotonic.hrl").

%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(all, #m{value=undefined} = M, _Context) ->
    M#m{value=all};
m_find_value(Key, #m{value=all}, Context) ->
    get_all(Key, Context);
m_find_value(Key, #m{value=undefined}, Context) ->
    get(Key, Context).

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context)
m_to_list(#m{value=undefined}, Context) ->
    all(Context).

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, Context) ->
    all(Context).


%% @doc Return the complete site configuration
all(Context) ->
    F = fun() ->
        z_sites_sup:get_site_config(Context#context.host)
    end,
    z_depcache:memo(F, site_config, Context).

%% @doc Fetch a key from the site configuration
get(Key, Context) when is_atom(Key) ->
    case z_depcache:get(site_config, Key, Context) of
        {ok, undefined} ->
            undefined;
        {ok, Cs} ->
            Cs;
        undefined ->
            All = all(Context),
            proplists:get_value(Key, All)
    end.

%% @doc Fetch a nested key from the site configuration
get(Module, Key, Context) when is_atom(Key) ->
	case get(Module, Context) of
		undefined -> undefined;
		L when is_list(L) -> proplists:get_value(Key, L)
	end.

%% @doc Fetch all values for a key, eg for the hostalias key.
get_all(Key, Context) ->
    proplists:get_all_values(Key, all(Context)).
