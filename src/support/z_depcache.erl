%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2010  Marc Worrell, 2014 Arjan Scherpenisse
%%
%% @doc z_depcache interface file for handing depcache functions from
%% the Zotonic context.

%% Copyright 2009-2010 Marc Worrell, 2014 Arjan Scherpenisse
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

-module(z_depcache).
-author("Marc Worrell <marc@worrell.nl>").

%% gen_server API
-export([start_link/1]).

%% depcache exports
-export([set/3, set/4, set/5, get/2, get_wait/2, get/3, get_subkey/3, flush/2, flush/1, size/1]).
-export([memo/2, memo/3, memo/4, memo/5]).
-export([in_process/0, in_process/1, flush_process_dict/0]).


-include_lib("zotonic.hrl").


%% @doc Start depcache instance based on site configuration
start_link(SiteProps) -> 
    Host = proplists:get_value(host, SiteProps),
    Name = z_utils:name_for_host(?MODULE, Host),
    depcache:start_link(Name, [{memory_max, proplists:get_value(depcache_memory_max, SiteProps)}]).


memo(Function, #context{depcache=Server}) ->
    depcache:memo(Function, Server).

memo(Function, MaxAge, #context{depcache=Server}) when is_tuple(Function) ->
    depcache:memo(Function, undefined, MaxAge, [], Server);

memo(F, Key, #context{depcache=Server}) when is_function(F) ->
    depcache:memo(F, Key, ?HOUR, [], Server).

memo(F, Key, MaxAge, #context{depcache=Server}) ->
    depcache:memo(F, Key, MaxAge, [], Server).

memo(F, Key, MaxAge, Dep, #context{depcache=Server}) ->
    depcache:memo(F, Key, MaxAge, Dep, Server).

%% @spec set(Key, Data, Context) -> void()
%% @doc Add the key to the depcache, hold it for 3600 seconds and no dependencies
set(Key, Data, #context{depcache=Server}) ->
    depcache:set(Key, Data, 3600, [], Server).

%% @spec set(Key, Data, MaxAge, Context) -> void()
%% @doc Add the key to the depcache, hold it for MaxAge seconds and no dependencies
set(Key, Data, MaxAge, #context{depcache=Server}) ->
    depcache:set(Key, Data, MaxAge, [], Server).

%% @spec set(Key, Data, MaxAge, Depend, Context) -> void()
%% @doc Add the key to the depcache, hold it for MaxAge seconds and check the dependencies
set(Key, Data, MaxAge, Depend, #context{depcache=Server}) ->
    depcache:set(Key, Data, MaxAge, Depend, Server).


%% @spec get_wait(Key, Context) -> {ok, Data} | undefined
%% @doc Fetch the key from the cache, when the key does not exist then lock the entry and let 
%% the calling process insert the value. All other processes requesting the key will wait till
%% the key is updated and receive the key's new value.
get_wait(Key, #context{depcache=Server}) ->
    depcache:get_wait(Key, Server).


%% @spec get(Key, Context) -> {ok, Data} | undefined
%% @doc Fetch the key from the cache, return the data or an undefined if not found (or not valid)
get(Key, #context{depcache=Server}) ->
    depcache:get(Key, Server).


%% @spec get_subkey(Key, SubKey, Context) -> {ok, Data} | undefined
%% @doc Fetch the key from the cache, return the data or an undefined if not found (or not valid)
get_subkey(Key, SubKey, #context{depcache=Server}) ->
    depcache:get_subkey(Key, SubKey, Server).


%% @spec get(Key, SubKey, Context) -> {ok, Data} | undefined
%% @doc Fetch the key from the cache, return the data or an undefined if not found (or not valid)
get(Key, SubKey, #context{depcache=Server}) ->
    depcache:get(Key, SubKey, Server).


%% @spec flush(Key, #context{}) -> void()
%% @doc Flush the key and all keys depending on the key
flush(Key, #context{depcache=Server}) ->
    depcache:flush(Key, Server).


%% @spec flush(#context{}) -> void()
%% @doc Flush all keys from the caches
flush(#context{depcache=Server}) ->
    depcache:flush(Server).


%% @spec size(#context{}) -> int()
%% @doc Return the total memory size of all stored terms
size(#context{depcache=Server}) ->
    depcache:size(Server).


%% @doc Check if we use a local process dict cache
in_process() ->
    depcache:in_process().

%% @doc Enable or disable the in-process caching using the process dictionary
in_process(Flag) ->
    depcache:in_process(Flag).

%% @doc Flush all items memoized in the process dictionary.
flush_process_dict() ->
    depcache:flush_process_dict().
