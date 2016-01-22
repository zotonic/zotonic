Browser/server interaction
==========================

There are multiple ways to set up interaction between server-side Zotonic code
and client-side JavaScript.

If you want to initiate the interaction in the client (browser)

1. wired events
2. notifications
3. API methods
4. transport
5. publish/subscribe.

.. _guide-named-wire:

Wired events
------------

First, define a named action:

.. code-block:: django

    {% wire name="test" action={growl text="Hello world"} %}

Then, call that action from your JavaScript code:

..  code-block:: javascript

    z_event("test");

Trigger notifications from JavaScript
-------------------------------------

Trigger a :ref:`notification <guide-notification>` in Zotonic with the
``z_notify`` JavaScript function:

.. code-block:: javascript

    z_notify("mymessage");

or:

.. code-block:: javascript

    z_notify("mymessage", {foo: bar, who: "world"});

This will trigger a call to::

    z_notifier:first(#postback_notify{message="mymessage"}, Context)

Which you can handle in any Zotonic module by defining::

    -export([observe_postback_notify/2]).
    observe_postback_notify(#postback_notify{message="mymessage"}, Context) ->
        Who = z_context:get_q(who, Context),
        z_render:growl("Hello " ++ Who, Context);
    observe_postback_notify(_, _Context) ->
        undefined.

All arguments are available via the ``z_context:get_q/2`` function (and friends).

Calling API services
--------------------

A third way is to write your own :ref:`API services <guide-services>` and use
standard JavaScript to perform Ajax GET/POST requests from the browser.

This use is perfectly possible and legal, although the other methods are
preferred, as they integrate nicely with the notification and action systems.
The API is more targeted to other applications interfacing to Zotonic.

.. _guide-transport:

Transport
---------

The transport functions are a low-level layer, just above the WebSocket, Comet
and AJAX methods used for communication between the server and the browser.

Zotonic has a message bus to transport data between server and browser. It
transports structured data in different formats and supports retransmission in
case of lost messages.

From browser to server
^^^^^^^^^^^^^^^^^^^^^^

To send a message from the browser to the server:

.. code-block:: javascript

    z_transport("mod_example", "ubf", {hello: "world"});

And then on the server, use Erlang to process the message::

    -module(mod_example).

    -export([event/2]).

    -include_lib("zotonic.hrl").

    event(#z_msg_v1{data=Data}, Context) ->
        io:format("~p", [Data]),
        Context;

This will print on the console::

    [{<<"hello">>,<<"world">>}]

Quality of service
^^^^^^^^^^^^^^^^^^

The message will be sent with a quality of service of 0. That means the browser
will try to send the message, but will not check if it arrived. Alterntively,
you can send with a qos of 1, in that case the browser will wait for an ack,
and if that doesn’t arrive in 30 seconds, then a duplicate message will be
requeued for transport:

.. code-block:: javascript

    z_transport("mod_example", "ubf", {hello: "world"}, {qos: 1});

It is possible to define a callback function that will be called if an ack is
received:

.. code-block:: javascript

    z_transport("mod_example", "ubf", {hello:"world"}, {
        qos: 1,
        ack: function(ackMsg, callOptions) {
            alert(ackMsg);
        }
    });

From server to browser
^^^^^^^^^^^^^^^^^^^^^^

Sending JavaScript (or other data) from the server to the browser is
straightforward::

    z_transport:page(javascript, <<"alert('Hello World');">>, Context);

This transports the JavaScript to the page associated with ``Context``. This
JavaScript will then be evaluated in the browser.

The default quality of service is 0 (see above); to let the page queue retry
delivering the message it is possible to specify another quality of service::

    z_transport:page(javascript, <<"alert('Hello World');">>, [{qos, 1}], Context);

It is also possible to send a message to all open pages of a session, or to all
sessions of a user::

    z_transport:session(javascript, <<"alert('Hello World');">>, [{qos, 1}], Context);
    z_transport:user(javascript, <<"alert('Hello World');">>, [{qos, 1}], Context);

Or transport to a specific page, session or user, but then you will need to
specify the message and the message-queue::

    Msg = z_transport:msg(session, javascript, <<"alert('Hello World');">>, [{qos, 1}]).
    z_transport:transport_user(Msg, UserId, Context).

The message queue is either ``session`` or ``page``. It defines which queue will
be responsible for resending the message and where the ack message is received.
If ``user`` is specified as queue then it will be replaced by ``session``.

.. seealso:: :ref:`transport reference <ref-transport>`.

Publish/subscribe (PubSub)
--------------------------

It is possible to publish and subscribe to topics on the server. Messages are
relayed between the server and the browser.

See :ref:`mod_mqtt` for more information.

An example of MQTT PubSub usage is the custom tag :ref:`scomp-live`.

Transport mechanisms
--------------------

Zotonic uses various mechanisms to transport data between the browser and the server:

 * AJAX callbacks to the server using :ref:`controller-postback`
 * WebSocket with bidirectional transports using :ref:`controller-websocket`
 * Comet transporting data from the server to the browser using :ref:`controller-comet`
 * HTML form posts to :ref:`controller-postback`.

AJAX calls also transport back data from the server to the browser.

