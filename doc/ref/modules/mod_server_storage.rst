
.. include:: meta-mod_server_storage.rst

Server side storage for the client (aka browser) and server.

The storage is tied to the ``sid`` in the authentication token of the client.
This ``sid`` is unique and changed if the user logs on or logs off.


Model functions
---------------

``model/server_storage/post/key`` store a key, with the value of in the payload of the message.

``model/server_storage/get/key`` - fetch a key.

``model/server_storage/get`` - fetch all keys.

``model/server_storage/delete/key`` - delete a key.

``model/server_storage/delete`` - delete all values from the server storage.

.. highlight:: javascript

Example in JavaScript from the client::

    cotonic.broker.publish("bridge/origin/model/server_storage/post/foo", { "hello": "world" });

    cotonic.broker.call("bridge/origin/model/server_storage/get/foo")
           .then( (msg) => console.log(msg) );

    {
        dup: false
        packet_id: null
        payload: {result: {hello: "world"}, status: "ok"}
        properties: {content_type: "application/json"}
        qos: 0
        retain: false
        topic: "reply/page-3-0.4080048813145437"
        type: "publish"
    }


.. highlight:: django

Example in a template::

    The hello key of foo is now: {{ m.server_storage.foo.hello|escape }}

Note that we **must** escape the value, as it originates from the client and contains
arbitrary values, including HTML.


.. highlight:: erlang

Example in Erlang::

    m_server_storage:store(<<"foo">>, #{ <<"hello">> => <<"world">> }, Context),

    case m_server_storage:lookup(<<"foo">>, Context) of
        {ok, Value} ->  % Found the value
        {error, no_session} -> % No session
        {error, not_found} -> % There is a session but unknown key
    end.

    m_server_storage:delete(<<"foo">>, Context).


Storage of secret server data
-----------------------------

Sometimes the server code wants to attach data to the specific client that is not accessible
to the client itself. An example is a secret during an OAuth handshake.

.. highlight:: erlang

For this there is a special, unlimited, storage API, which is only accessible using the
Erlang API::

    m_server_storage:secure_store(<<"foo">>, #{ <<"my">> => <<"secret">> }, Context),

    case m_server_storage:secure_lookup(<<"foo">>, Context) of
        {ok, Value} ->  % Found the value
        {error, no_session} -> % No session
        {error, not_found} -> % There is a session but unknown key
    end.

    m_server_storage:secure_delete(<<"foo">>, Context).

The keys in the *secure* storage are separate from the keys in the normal storage.
There can be a key with the same name and different values in both storages.


Storage process
---------------

If a value is stored in the server side storage then a process is started.
This process holds all stored values, and times out after 900 seconds.

Any HTTP request with the proper ``sid`` will extend the lifetime of the
storage process.

The storage process holds at most 100 KB of data, above that it will respond
with a ``full`` error status.


Configuration keys
------------------

There are two configuration keys, both need a number:

 * ``mod_server_storage.storage_expire`` The timeout in seconds of the storage process
   defaults to 900 seconds.
 * ``mod_server_storage.storage_maxsize`` The maximum stored size in bytes, both the keys
   and the values are counted. Default is 100 KB.

Note that the storage is in-memory, so it is best to set the storage maxsize and expire as
low as possible.
