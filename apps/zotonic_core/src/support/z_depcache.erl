%% @author Arjan Scherpenisse
%% @copyright 2009-2025 Arjan Scherpenisse
%% @doc z_depcache interface file for handing depcache functions from
%% the Zotonic context.
%% @end

%% Copyright 2009-2025 Arjan Scherpenisse
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
-export([set/3, set/4, set/5, get/2, get_wait/2, get/3, get_subkey/3, flush/2, flush/1, size/1, record_depcache_event/2]).
-export([memo/2, memo/3, memo/4, memo/5]).
-export([in_process_server/1, in_process/1, flush_process_dict/0]).


-include("../../include/zotonic.hrl").


%% @doc Start depcache instance based on site configuration
start_link(SiteProps) ->
    Site = proplists:get_value(site, SiteProps),
    Name = z_utils:name_for_site(?MODULE, Site),
    depcache:start_link(
        Name,
        [
            {memory_max, proplists:get_value(depcache_memory_max, SiteProps)},
            {callback, {?MODULE, record_depcache_event, [Site]}}
        ]
    ).


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

%% @doc Add the key to the depcache, hold it for 3600 seconds and no dependencies
-spec set(Key, Data, Context) -> ok when
    Key :: any(),
    Data :: any(),
    Context :: z:context().
set(Key, Data, #context{depcache=Server}) ->
    depcache:set(Key, Data, 3600, [], Server).

%% @doc Add the key to the depcache, hold it for MaxAge seconds and no dependencies
-spec set(Key, Data, MaxAge, Context) -> ok when
    Key :: any(),
    Data :: any(),
    MaxAge :: non_neg_integer(),
    Context :: z:context().
set(Key, Data, MaxAge, #context{depcache=Server}) ->
    depcache:set(Key, Data, MaxAge, [], Server).

%% @doc Add the key to the depcache, hold it for MaxAge seconds and check the dependencies
-spec set(Key, Data, MaxAge, Depend, Context) -> ok when
    Key :: any(),
    Data :: any(),
    MaxAge :: non_neg_integer(),
    Depend :: [ Key ],
    Context :: z:context().
set(Key, Data, MaxAge, Depend, #context{depcache=Server}) ->
    depcache:set(Key, Data, MaxAge, Depend, Server).


%% @doc Fetch the key from the cache, when the key does not exist then lock the entry and let
%% the calling process insert the value. All other processes requesting the key will wait till
%% the key is updated and receive the key's new value.
-spec get_wait(Key, Context) -> Result when
    Key :: any(),
    Context :: z:context(),
    Data :: any(),
    Result :: {ok, Data}
            | undefined
            | {throw, term()}
            | {error, term()}.
get_wait(Key, #context{depcache=Server}) ->
    depcache:get_wait(Key, Server).


%% @doc Fetch the key from the cache, return the data or an undefined if not found (or not valid)
-spec get(Key, Context) -> {ok, Data} | undefined when
    Key :: any(),
    Context :: z:context(),
    Data :: any().
get(Key, #context{depcache=Server}) ->
    depcache:get(Key, Server).


%% @doc Fetch the key from the cache, return the data or an undefined if not found (or not valid)
-spec get_subkey(Key, SubKey, Context) -> {ok, Data} | undefined when
    Key :: any(),
    SubKey :: any(),
    Context :: z:context(),
    Data :: any().
get_subkey(Key, SubKey, #context{depcache=Server}) ->
    depcache:get_subkey(Key, SubKey, Server).


%% @doc Fetch the key from the cache, return the data or an undefined if not found (or not valid)
-spec get(Key, SubKey, Context) -> {ok, Data} | undefined when
    Key :: any(),
    SubKey :: any(),
    Context :: z:context(),
    Data :: any().
get(Key, SubKey, #context{depcache=Server}) ->
    depcache:get(Key, SubKey, Server).


%% @doc Flush the key and all keys depending on the key
-spec flush(Key, Context) -> ok when
    Key :: any(),
    Context :: z:context().
flush(Key, #context{depcache=Server}) ->
    depcache:flush(Key, Server).


%% @doc Flush all keys from the caches
-spec flush(Context) -> ok when
    Context :: z:context().
flush(#context{depcache=Server}) ->
    depcache:flush(Server).


%% @doc Return the total memory size of all stored terms
-spec size(Context) -> non_neg_integer() | undefined when
    Context :: z:context().
size(#context{depcache=Server}) ->
    depcache:size(Server).


%% @doc Check if we use a local process dict cache
-spec in_process_server(Server) -> boolean() when
    Server :: pid() | atom().
in_process_server(Server) ->
    depcache:in_process_server(Server).

%% @doc Enable or disable the in-process caching using the process dictionary
-spec in_process(boolean() | undefined) -> boolean() | undefined.
in_process(Flag) ->
    depcache:in_process(Flag).

%% @doc Flush all items memoized in the process dictionary.
-spec flush_process_dict() -> ok.
flush_process_dict() ->
    depcache:flush_process_dict().

record_depcache_event({eviction, _DepcacheName}, Host) ->
    z_stats:record_event(depcache, eviction, Host);
record_depcache_event(_Event, _Metadata) ->
    ok.
