
.. include:: meta-mod_mqtt.rst

Enables MQTT support in a site.

MQTT is a machine-to-machine (M2M)/“Internet of Things” connectivity protocol. 
It was designed as an extremely lightweight publish/subscribe messaging transport. 
It is useful for connections with remote locations where a small code footprint is required and/or network bandwidth is at a premium. For example, it has been used in sensors communicating to a broker via satellite link, over occasional dial-up connections with healthcare providers, and in a range of home automation and small device scenarios (source and more information: `MQTT.org <http://mqtt.org>`_)

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
|test                             |Test topic. If you publish here then mod_mqtt will log debug message.    |
+---------------------------------+-------------------------------------------------------------------------+
|user                             |Topic available for any authenticated user                               |
+---------------------------------+-------------------------------------------------------------------------+
|site/sitename                    |Root for a site                                                          |
+---------------------------------+-------------------------------------------------------------------------+
|site/sitename/public             |Freely accessible within the site                                        |
+---------------------------------+-------------------------------------------------------------------------+
|site/sitename/test               |Test topic, freely accessible within the site                            |
+---------------------------------+-------------------------------------------------------------------------+
|site/sitename/user               |Topic available for any authenticated user of the site *sitename*        |
+---------------------------------+-------------------------------------------------------------------------+
|site/sitename/user/UserId        |Topic available for a specific user of the site *sitename*               |
+---------------------------------+-------------------------------------------------------------------------+
|site/sitename/session            |HTML pages can subscribe, admins can publish                             |
+---------------------------------+-------------------------------------------------------------------------+
|site/sitename/session/SessionId  |Topic to relay information to a specific session                         |
+---------------------------------+-------------------------------------------------------------------------+
|site/sitename/pagesession        |HTML pages can subscribe, admins can publish                             |
+---------------------------------+-------------------------------------------------------------------------+
|site/sitename/pagesession/PageId |Topic to relay information to a specific page (in a browser)             |
+---------------------------------+-------------------------------------------------------------------------+


Topics and namespaces
^^^^^^^^^^^^^^^^^^^^^

To make it easier to write generic software, without changing topic names, some namespace conventions and mappings are introduced.

The following topics are expanded

+--------------------------+------------------------------------------------------------------------------------------------------------+
|Topic                     | Expansion                                           | Description                                          |
+==========================+=====================================================+======================================================+
| ~site                    | site/mysite                                         | The context’s site root topic                        |
+--------------------------+-----------------------------------------------------+------------------------------------------------------+
| ~session                 | site/mysite/session/oLfVVaT299zpSjlGb5Im            | The topic for the current session                    |
+--------------------------+-----------------------------------------------------+------------------------------------------------------+
| ~pagesession             | site/mysite/pagesession/vWCUKL9QKmfLxotWorZv        | The topic for the current HTML page in the browser   |
+--------------------------+-----------------------------------------------------+------------------------------------------------------+
| ~user                    | site/user/1234                                      | The topic for the current user                       |
+--------------------------+-----------------------------------------------------+------------------------------------------------------+

Note that there are not automatic subscriptions for session, pagesession and user topics. All subscriptions need to be added explicitly.


Access control
^^^^^^^^^^^^^^

All topics have access control added. For this an extra ACL object ``#acl_mqtt{}`` defined, with the actions ``publish`` and ``subscribe``.
Modules can observe the usual ``#acl_is_allowed{}`` notification to add access control to topics.


Subscribing modules
^^^^^^^^^^^^^^^^^^^

Modules can automatically subscribe to topics. This is done by adding specially named functions.

For example, the following function subscribes to the topic ``site/sitename/test``::

    -export([
        'mqtt:~site/test'/3
    ]).

    'mqtt:~site/test'(Message, ModulePid, Context) ->
        lager:debug("mqtt:~site/test received: ~p", [{Message, ModulePid, z_context:site(Context)}]),
        ok.

Here *Message* is the received ``#mqtt_msg{}``, and *ModulePid* is the process id of the running module.

The function will be called from within a process that is subscribed to the topic.

Erlang API
^^^^^^^^^^

Subscribe a function F in a module M to a topic::

    -spec subscribe(binary()|string(), {atom,atom}, #context{}) -> ok | {error, eacces}.
    z_mqtt:subscribe(Topic, {M,F}, Context)

This starts a process that will subscribe to the topic and call the function whenever a message is received.
The topic can have wildcards, though access control applies and the result ``{error, eacces}`` will be returned if access is denied, ``ok`` will be returned on a succesful subscription.

Subscribe the current process to a topic::

    -spec subscribe(binary()|string(), #context{}) -> ok | {error, eacces}.
    z_mqtt:subscribe(Topic, Context)

When the process stops it will automatically be unsubscribed.
The process will receive messages ``{route, #mqtt_msg{}}``.

Subscribe another process to a topic::

    -spec subscribe(binary()|string(), pid(), #context{}) -> ok | {error, eacces}.
    z_mqtt:subscribe(Topic, Pid, Context)

To unsubscribe, use ``z_mqtt:unsubscribe`` with the same arguments as during subscription.

To publish a message::

        -spec publish(binary()|string(), term(), #context{}) -> ok | {error, eacces}.
        z_mqtt:publish(Topic, SomeData, Context)


JavaScript API
^^^^^^^^^^^^^^

The JavaScript API uses callback functions:

.. code-block:: javascript

	pubzub.subscribe("~pagesession/foo/#", function(topic, msg) { console.log(topic, msg); });
	pubzub.publish("~pagesession/foo/bar", "hello world");

If the received message was relayed from the server then it is an object:

.. code-block:: javascript

    {
    	ubf_type: ubf.TUPLE,
    	_record: "z_mqtt_payload",
    	version: 1,
    	site: "yoursitename",
    	user_id: 23,			// The id of the user sending the message
    	encoding: "ubf",		// The way the payload was encoded
    	payload: ...			// The decoded payload
    }

The transport between the server and the browser always uses UBF(A).
Most decoded values will be an objecy with an extra ``ubf_type``, always use the method ``.valueOf()`` to get the primitive type of the object.

You will need to include the following JavaScript files:

.. code-block:: django

	{% lib
		 "js/qlobber.js"
		 "js/pubzub.js"
	%}

The file ``js/modules/ubf.js`` should already have been included, as it is used by ``zotonic-1.0.js``.


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

MQTT can listen on a port for incoming connections. Per default the listener is disabled.

Configuration
^^^^^^^^^^^^^

The MQTT listener is configured in the :file:`priv/erlang.config`.
If this file is missing then it can be copied from :file:`priv/erlang.config.in`.

The following section defines the Zotonic authentication module, access control, and a listener on the standard MQTT port 1883:

.. code-block:: erlang

   {emqtt, [
      {auth, {z_mqtt_auth, []}},
      {access_control, {zotonic, []}},
      {listeners, [
          {1883,  [
              binary,
              {packet,        raw},
              {reuseaddr,     true},
              {backlog,       128},
              {nodelay,       true}
          ]}
      ]}
   ]},


Authentication
^^^^^^^^^^^^^^

All connections must authenticat themselves using an username and password.
The username is postfixed with the hostname of the user’s site, for example: ``jantje@foobar.com``.
In this way Zotonic knows which site the user belongs to. 

If no matching site can be found, or if no hostname is given, then Zotonic 
will try to authenticate against the default site.


Debugging
^^^^^^^^^

To see which topics are being subscribed and published to, you can
configure ``mod_mqtt`` to print debug messages on the Zotonic console
whenever a publish or subscribe occurs.

To do so, go to the modules overview in the admin interface, and
scroll down to ``mod_mqtt``. Then, click on the "configure" button in
the row in the right and tick the checkbox to enable these debug
messages.

.. image:: /img/mod_mqtt_config.png
