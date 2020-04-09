
.. include:: meta-mod_mqtt.rst

`MQTT <http://mqtt.org>`_ is a machine-to-machine (M2M)/“Internet of Things”
connectivity protocol. It was designed as an extremely lightweight
publish/subscribe messaging transport. It is useful for connections with remote
locations where a small code footprint is required and/or network bandwidth
is at a premium. For example, it has been used in sensors communicating to a
broker via satellite link, over occasional dial-up connections with healthcare
providers, and in a range of home automation and small device scenarios

MQTT uses a simple message broker to route messages from publishers to multiple subscribers.

A quick overview of MQTT
------------------------

Publish/subscribe
^^^^^^^^^^^^^^^^^

With MQTT messages are published to topics. All subscribers to a topic will then receive the message.
A topic is a string, much like a file-path, for example: ``truck/0001/temperature``

A subscriber can directly subscribe to a topic, or can use wildcards to subscribe to related topics.
For this the wildcards ``+`` and ``#`` can be used.  ``+`` matches exactly one word between the slashes,
and ``#`` can be used at the end of a pattern to match all sub-topics.

Examples of subscription patterns:

 * ``truck/0001/temperature`` matches the temperature publications of truck 0001.
 * ``truck/+/temperature`` matches all temperature publications for all trucks.
 * ``+/+/temperature`` matches all temperature publications for all trucks and other things with a temperature
 * ``truck/0001/#`` matches all publishes to ``truck/0001`` and all its sub-topics

Retained messages
^^^^^^^^^^^^^^^^^

A publisher can publish a *retained* message to a topic. When publishing all current topic subscribers will receive the message.
Above that, if a new subscription is made to the topic, then all retained messages are sent to the new subscriber.

Quality of service
^^^^^^^^^^^^^^^^^^

MQTT has three levels for the quality of message delivery. These are used when sending messages between machines.
The levels are:

 * Level 0: send the message, no reception acknowledgments are reported.
 * Level 1: on receipt a single ack is sent back to the publisher
 * Level 2: a double handshake is performed

For most communication level 0 is used.

Wills
^^^^^

A client can set a *last will* message and topic. This is a message that will be published to the topic at the moment the client is unexpectedly disconnected.


MQTT in Zotonic
---------------

Zotonic has a central MQTT message broker. Optionally clients can connect to this broker using the normal MQTT protocol.

The broker is used for internal publish/subscribe support.

Each open HTML page can also have a local (simplified) broker. The system can relay messages between the brokers on open pages and the central broker in Zotonic.
In this way it is possible for HTML pages to have their own local publish/subscribe system and also subscribe or publish to topics on the central broker.

As the central broker is shared between sites it is even possible to publish/subscribe between different sites. In the future it will be possible to bridge the brokers between servers.

Predefined topics
^^^^^^^^^^^^^^^^^

Currently the following topics are defined:

+---------------------------------+-------------------------------------------------------------------------+
|Topic                            |Description                                                              |
+=================================+=========================================================================+
|public                           |Freely accessible topic, both for subscribe and publish                  |
+---------------------------------+-------------------------------------------------------------------------+
|test                             |Test topic. If you publish here then mod_mqtt will log a debug message.  |
+---------------------------------+-------------------------------------------------------------------------+
|user                             |Topic available for any authenticated user                               |
+---------------------------------+-------------------------------------------------------------------------+
|user/UserId                      |Topic available for a specific user of the site                          |
+---------------------------------+-------------------------------------------------------------------------+
|bridge/ClientId                  |The topic forwarding to the client with id ClientId                      |
+---------------------------------+-------------------------------------------------------------------------+


Topics and namespaces
^^^^^^^^^^^^^^^^^^^^^

To make it easier to write generic software, without changing topic names, some
namespace conventions and mappings are introduced.

The following topics are expanded:

+--------------------------+-----------------------------------------------------+------------------------------------------------------+
|Topic                     | Expansion                                           | Description                                          |
+==========================+=====================================================+======================================================+
| ~client                  | bridge/vWCUKL9QKmfLxotWorZv                         | The bridge topic that forwards to the user agent     |
+--------------------------+-----------------------------------------------------+------------------------------------------------------+
| ~user                    | user/1234 *or* user/anonymous                       | The topic for the current user                       |
+--------------------------+-----------------------------------------------------+------------------------------------------------------+

Note that there are not automatic subscriptions for user topics. All subscriptions need to be added explicitly.

.. _mqtt-access-control:

Access control
^^^^^^^^^^^^^^

All topics have access control added. For this an extra ACL object
:ref:`#acl_mqtt{} <acl_mqtt>` is defined, with the actions ``publish`` and
``subscribe``. Modules can observe the usual :ref:`acl_is_allowed` notification
to allow access to MQTT topics:

.. code-block:: erlang
    :caption: your_site.erl

    observe_acl_is_allowed(#acl_is_allowed{object = #acl_mqtt{topic = [ <<"my">>, <<"topic">> ]}}, _Context) ->
        %% Allow anonymous access on this topic
        true;
    observe_acl_is_allowed(#acl_is_allowed{}, _Context) ->
        undefined.

Subscribing modules
^^^^^^^^^^^^^^^^^^^

Modules can automatically subscribe to topics. This is done by adding specially named functions.

For example, the following function subscribes to the topic ``test/#``::

    -export([
        'mqtt:test/#'/2
    ]).

    -spec 'mqtt:test/#'( map(), z:context() ) -> ok.
    'mqtt:test/#'(Message, Context) ->
        lager:debug("mqtt:test on site ~p received ~p", [ z_context:site(Context), Message ]),
        ok.

Here *Message* is a map with the received MQTT message (of type ``publish``)::

    #{
        type => publish,
        pool => Pool,                 % A MQTT pool (and topic tree) per site
        topic => Topic,               % Unpacked topic for the publish [ <<"foo">>, <<"bar">> ]
        topic_bindings => Bound,      % Variables bound from the topic
        message => Msg,               % The MQTT message itself
        publisher_context => PublisherContext
    }

The *Context* is the context of the process/user that subscribed to the message. Use the ``publisher_context``
for the *Context* (and ACL permissions) of the publisher.

Erlang API
^^^^^^^^^^

Subscribe a function F in a module M to a topic::

    -spec subscribe(z_mqtt:topic(), mfa(), pid(), z:context()) -> ok | {error, eacces | term()}.
    z_mqtt:subscribe([ <<"my">>, <<"topic">>, '#' ], {M, F, []}, self(), Context)

This will subscribe the function, with the current process (``self()``) as the managing process.
If the process exits then the subscription is removed.

Access control applies and the result ``{error, eacces}`` will be returned
if access is denied, ``ok`` will be returned on a succesful subscription.

Subscribe the current process to a topic::

    -spec subscribe(z_mqtt:topic(), z:context()) -> ok | {error, eacces | term()}.
    z_mqtt:subscribe(Topic, Context)

When the process stops it will automatically be unsubscribed.
The process will receive messages ``{mqtt_msg, map()}``, where the ``map()`` is like
the map in the section above.

Subscribe another process to a topic::

    -spec subscribe(z_mqtt:topic(), pid(), z:context()) -> ok | {error, eacces | term()}.
    z_mqtt:subscribe(Topic, Pid, Context)

To unsubscribe, use ``z_mqtt:unsubscribe`` with the same arguments as during subscription.

To publish a message::

    -spec publish( z_mqtt:topic(), term(), z:context() ) -> ok | {error, term()}.
    z_mqtt:publish(Topic, Payload, Context)

With options (``qos`` or ``retain``)::

    -spec publish( z_mqtt:topic(), term(), z_mqtt:publish_options(), z:context() ) -> ok | {error, term()}.
    z_mqtt:publish(Topic, Payload, #{ qos => 1, retain => true }, Context)

Or, with a complete MQTT message::

    -spec publish( mqtt_packet_map:mqtt_packet(), z:context()) -> ok | {error, term()}.
    Msg = #{
        type => publish,
        qos => 0,
        topic => [ <<"~client">>, <<"public">>, <<"hello">> ]
        payload = #{ key => 1, foo => <<"bar">> }
    },
    z_mqtt:publish(Msg, Context)


JavaScript API
^^^^^^^^^^^^^^

There is a separate topic tree in the browser. To be able to send message from/to the browser
there are special *bridge* topics on both ends.

The browser receives an unique client and routing id on connecting to the server. On the server
those ids can be used to route messages back to the client using a bridge topic.

For example the server side topic::

    bridge/MyClientId/browser/topic

Is mapped on the client to::

    browser/topic

It is possible to send messages to the server, or subscribe to topics on the server. For this there
is a special ``bridge/origin`` (the *bridge to origin*, ie. the server serving the page) topic.

Any subscribe or publish action on this topic is relayed to the server. For example, to access the
server side topic ``my/server/topic``, use the client side topic ``bridge/origin/server/topic`` (both
for publish and subscribe).

The JavaScript API uses callback functions:

.. code-block:: javascript

	cotonic.broker.subscribe("bridge/origin/foo/#", function(msg, bindings, options) { console.log(msg); });
	cotonic.broker.publish("bridge/origin/foo/bar", "hello world");

The received message is an JSON object:

.. code-block:: javascript

    {
      type: "publish",
      qos: 0,
      payload: "hello world",
      properties: {
          ...
      },
      ...
    }

The transport between the server and the browser uses a websocket connection and binary encoded MQTT v5
messages.


Connection will
^^^^^^^^^^^^^^^

Currently a simple version of the *lastwill* is available for JavaScript.  This sets a topic and message to be sent when the page process stops.

Multiple wills can be set. Currently it is not possible to remove a will, though that will change in the near future.

Example:

.. code-block:: javascript

    var will_id = pubzub.lastwill("~site/goodbye", "thanks for the fish");


Quality of service
^^^^^^^^^^^^^^^^^^

Currently there is no quality of service implemented for the JavaScript API and relay. The server side page process will buffer all messages till the browser connects to the page session. This happens on connects with comet, WebSocket, and postbacks.

On the browser all messages are queued and sent one by one to the server. This uses either the WebSocket connection or the postback interface.


Enabling the MQTT listener
--------------------------

MQTT can listen on a port for incoming connections. Per default the listener is enabled.

Configuration
^^^^^^^^^^^^^

The MQTT listener is configured in the :file:`~.zotonic/zotonic.config`.
If this file is missing then it can be copied from :file:`~apps/zotonic_launcher/priv/zotonic.config.in`.

Per default it listens on MQTT port 1883 and MQTT with TLS on port 8883::

    %%% IP address for MQTT connections - defaults to 'listen_ip'
    %%% Use 'none' to disable.
       %% {mqtt_listen_ip, any},

    %%% IPv6 address for MQTT connections - defaults to 'listen_ip6'
    %%% Use 'none' to disable.
       %% {mqtt_listen_ip6, any},

    %%% Port number for MQTT connections
       %% {mqtt_listen_port, 1883},

    %%% Port number for MQTT ssl connections
       %% {mqtt_listen_ssl_port, 8883},


Authentication
^^^^^^^^^^^^^^

All connections must authenticate using an username and password.
The username is prefixed with the hostname of the user’s site, for example: ``foobar.com:myusername``.
In this way Zotonic knows which site the user belongs to.

If no matching site can be found, or if no hostname is given, then Zotonic
will try to authenticate against the default site.


.. seealso::
    :ref:`live tag <scomp-live>`, which uses MQTT topics.
