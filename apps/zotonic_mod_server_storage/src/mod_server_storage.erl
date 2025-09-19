%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020-2025 Marc Worrell
%% @doc Server side sessions and data storage.
%% @end

%% Copyright 2020-2025 Marc Worrell
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

-module(mod_server_storage).
-moduledoc("
Server side storage for the client (aka browser) and server.

The storage is tied to the `sid` in the authentication token of the client. This `sid` is unique and changed if the user
logs on or logs off.



Model functions
---------------

`model/server_storage/post/key` store a key, with the value of in the payload of the message.

`model/server_storage/get/key` - fetch a key.

`model/server_storage/get` - fetch all keys.

`model/server_storage/delete/key` - delete a key.

`model/server_storage/delete` - delete all values from the server storage.

Example in JavaScript from the client:


```javascript
cotonic.broker.publish(\"bridge/origin/model/server_storage/post/foo\", { \"hello\": \"world\" });

cotonic.broker.call(\"bridge/origin/model/server_storage/get/foo\")
       .then( (msg) => console.log(msg) );

{
    dup: false
    packet_id: null
    payload: {result: {hello: \"world\"}, status: \"ok\"}
    properties: {content_type: \"application/json\"}
    qos: 0
    retain: false
    topic: \"reply/page-3-0.4080048813145437\"
    type: \"publish\"
}
```

Example in a template:


```django
The hello key of foo is now: {{ m.server_storage.foo.hello|escape }}
```

Note that we **must** escape the value, as it originates from the client and contains arbitrary values, including HTML.

Example in Erlang:


```erlang
m_server_storage:store(<<\"foo\">>, #{ <<\"hello\">> => <<\"world\">> }, Context),

case m_server_storage:lookup(<<\"foo\">>, Context) of
    {ok, Value} ->  % Found the value
    {error, no_session} -> % No session
    {error, not_found} -> % There is a session but unknown key
end.

m_server_storage:delete(<<\"foo\">>, Context).
```



Storage of secret server data
-----------------------------

Sometimes the server code wants to attach data to the specific client that is not accessible to the client itself. An
example is a secret during an OAuth handshake.

For this there is a special, unlimited, storage API, which is only accessible using the Erlang API:


```erlang
m_server_storage:secure_store(<<\"foo\">>, #{ <<\"my\">> => <<\"secret\">> }, Context),

case m_server_storage:secure_lookup(<<\"foo\">>, Context) of
    {ok, Value} ->  % Found the value
    {error, no_session} -> % No session
    {error, not_found} -> % There is a session but unknown key
end.

m_server_storage:secure_delete(<<\"foo\">>, Context).
```

The keys in the *secure* storage are separate from the keys in the normal storage. There can be a key with the same name
and different values in both storages.



Storage process
---------------

If a value is stored in the server side storage then a process is started. This process holds all stored values, and
times out after 900 seconds.

Any HTTP request with the proper `sid` will extend the lifetime of the storage process.

The storage process holds at most 100 KB of data, above that it will respond with a `full` error status.



Configuration keys
------------------

There are two configuration keys, both need a number:

*   `mod_server_storage.storage_expire` The timeout in seconds of the storage process defaults to 900 seconds.
*   `mod_server_storage.storage_maxsize` The maximum stored size in bytes, both the keys and the values are counted. Default is 100 KB.

Note that the storage is in-memory, so it is best to set the storage maxsize and expire as low as possible.
").
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Server Session Storage").
-mod_description("Server side data storage in sessions.").
-mod_prio(500).
-mod_depends([base]).
-mod_provides([server_storage]).
-mod_config([
        #{
            key => storage_expire,
            type => integer,
            default => 900,
            description => "The time in seconds after which the server side storage expires and is removed."
        },
        #{
            key => storage_maxsize,
            type => integer,
            default => 102400,
            description => "The maximum size of the server side storage in bytes. Above this size writes are refused."
        }
    ]).

-behaviour(supervisor).

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    observe_request_context/3,
    observe_server_storage/2,
    start_session/2,
    start_link/1,
    init/1
    ]).


%% @doc Periodic ping for the session, done by the client.
-spec observe_request_context(#request_context{}, z:context(), z:context()) -> z:context().
observe_request_context(#request_context{}, Context, _Context) ->
    m_server_storage:ping(Context),
    Context.

%% @doc Decoupling of server storage from other modules.
observe_server_storage({server_storage, Verb, Key}, Context) ->
    observe_server_storage({server_storage, Verb, Key, undefined}, Context);
observe_server_storage({server_storage, Verb, Key, OptValue}, Context) ->
    case Verb of
        lookup  -> m_server_storage:lookup(Key, Context);
        store   -> m_server_storage:store(Key, OptValue, Context);
        delete  -> m_server_storage:delete(Key, Context);
        secure_lookup  -> m_server_storage:secure_lookup(Key, Context);
        secure_store   -> m_server_storage:secure_store(Key, OptValue, Context);
        secure_delete  -> m_server_storage:secure_delete(Key, Context)
    end;
observe_server_storage({server_storage, Verb}, Context) ->
    case Verb of
        lookup  -> m_server_storage:lookup(Context);
        delete  -> m_server_storage:delete(Context);
        secure_lookup  -> m_server_storage:secure_lookup(Context);
        secure_delete  -> m_server_storage:secure_delete(Context)
    end.


%% @doc Start the session with the given Id
-spec start_session( binary(), z:context() ) -> {ok, pid()} | {error, term()}.
start_session(SessionId, Context) when is_binary(SessionId) ->
    Name = z_utils:name_for_site(?MODULE, Context),
    case supervisor:start_child(Name, [SessionId, z_context:new(Context) ]) of
        {ok, Pid} -> {ok, Pid};
        {error, {already_started, Pid}} -> Pid;
        {error, _} = Error -> Error
    end.

%%% ------------------------------------------------------------------------------------
%%% Supervisor callbacks
%%% ------------------------------------------------------------------------------------

start_link(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    Name = z_utils:name_for_site(?MODULE, Context),
    supervisor:start_link({local, Name}, ?MODULE, []).

init([]) ->
    Type = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 5
    },
    ChildSpecs = [
        #{
            id => session_process,
            start => {z_server_storage, start_link, []},
            type => worker,
            restart => temporary
        }
    ],
    {ok, {Type, ChildSpecs}}.
