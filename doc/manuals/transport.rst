.. _manual-transport:

Transporting data between browser and server
============================================

Zotonic has a message bus to transport data from the server to the browser and vice versa.
It can transport structured data in different formats and supports retransmission in case of lost messages.

Quick overview
--------------

Browser sends a message to a module
....................................

.. highlight: javascript

If you want to send a message from the browser to the server, in javascript do::

    z_transport("mod_example", "ubf", {hello:"world"});

.. highlight: erlang

And then on the server, using Erlang::

    -module(mod_example).

    -export([event/2]).

    -include_lib("zotonic.hrl").

    event(#z_msg_v1{data=Data}, Context) ->
        io:format("~p", [Data]),
        Context;

This will print on the console::

    [{<<"hello">>,<<"world">>}]

The message is defined as::

    -record(z_msg_v1, {
            qos = 0 :: 0 | 1 | 2,
            dup = false :: boolean(),
            msg_id :: binary(),
            timestamp :: pos_integer(),
            content_type = ubf :: text | javascript | json | form | ubf | atom() | binary(),
            delegate = postback :: postback | mqtt | atom() | binary(),
            push_queue = page :: page | session | user,

            % Set by transports from user-agent to server
            ua_class=undefined :: ua_classifier:device_type() | undefined,
            session_id :: binary(),
            page_id :: binary(),

            % Payload data
            data :: any()
        }).

.. highlight: javascript

The message will be sent with a *quality of service* of 0. That means the browser will try to send
the message, but will not check if it arrived. Alernatively you can send with a *qos* of 1, in that
case the browser will wait for an *ack*, and if that doesn't arrive in 30 seconds, then a duplicate
message will be requeued for transport::

    z_transport("mod_example", "ubf", {hello:"world"}, {qos:1});

It is possible to define a callback function that will be called if an ack is received::

    z_transport("mod_example", "ubf", {hello:"world"}, {
        qos: 1, 
        ack: function(ackMsg, callOptions) {
        }
    });

.. highlight: erlang

The ack message is defined as::

    -record(z_msg_ack, {
            qos = 1 :: 1 | 2,
            msg_id :: binary(),
            push_queue = page :: page | session | user,
            session_id :: binary(),
            page_id :: binary(),
            result :: any()
        }).


Send some javascript to the browser
...................................

Sending javascript (or other data) from the server to the browser is straight forward::

  z_transport:page(javascript, <<"alert('Hello World');">>, Context);

This transport the javascript to the page associated with the *Context*. On the browser this
javascript will be evaluated.

The default quality of service is 0, to let the page queue retry delivering the message it is possible to specify
another quality of service::

    z_transport:page(javascript, <<"alert('Hello World');">>, [{qos,1}], Context);

It is also possible to send a message to all open pages of a session, or to all sessions of an user::

    z_transport:session(javascript, <<"alert('Hello World');">>, [{qos,1}], Context);
    z_transport:user(javascript, <<"alert('Hello World');">>, [{qos,1}], Context);

Or transport to a specific page, session or user, but then you will need to specify the message and the message-queue::

    Msg = z_transport:msg(session, javascript, <<"alert('Hello World');">>, [{qos,1}]).
    z_transport:transport_user(Msg, UserId, Context).

The message-queue is either ``session`` or ``page``. It defines which queue will be responsible for resending the message and where
the ack message is received. If ``user`` is specified as queue then it will be replaced by ``session``.


Transport mechanisms
--------------------

Zotonic uses various mechanisms to transport data between the browser and the server:

 * AJAX callbacks to the server using :ref:`controller-postback`
 * Websocket with bidirectional transports using :ref:`controller-websocket`
 * Comet transporting data from the server to the browser using :ref:`controller-comet`
 * HTML Form posts to the :ref:`controller-postback`

AJAX calls also transport back data from the server to the browser.

