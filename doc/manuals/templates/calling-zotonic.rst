.. _manual-template-calling-zotonic:

Interactivity: Calling Zotonic from Javascript
----------------------------------------------

There are multiple ways to call Zotonic from Javascript: wired events, notifications, API methods, transport and PubSub.

Javascript is the lingua franca of web scripting.  Having a mechanism
of directly integrating Zotonic backend calls into a flow with JQuery
has great potential. This guide provides some pointers on how Zotonic
integrates with Javascript and jQuery.

You can use multiple ways to call Zotonic from JavaScript:

Wired events
^^^^^^^^^^^^

.. highlight: django

Use ``{% wire %}`` (see :ref:`scomp-wire`) to define a named
action and trigger it from Javascript with ``z_event("myname", { foo: bar,
... })``. 'foo' and other arguments will become query args (access
with z_context:get_q/2 and friends). For example::

  {% wire name="test" action={growl text="Hello World"} %} 

.. highlight: javascript

And then in some JS function::

  z_event("test"); 

or::

  z_event("test", { foo: bar });

Of course, you can also :ref:`scomp-wire` :ref:`postback actions <action-postback>`.

Trigger notification from Javascript
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Trigger a :ref:`notification <manual-notification>` in Zotonic with the ``z_notify`` function::

  z_notify("mymessage"); 

or::

  z_notify("mymessage", {foo: bar, lazy: "fox"});

.. highlight: erlang

This will trigger a call to::

  z_notifier:first(#postback_notify{message="mymessage"}, Context) 

Which you can handle in any zotonic module by defining::

  -export([ observe_postback_notify/2 ]). 
  observe_postback_notify(#postback_notify{message="mymessage"}, Context) -> 
      z_render:growl("Hello World", Context); 
  observe_postback_notify(_, _Context) -> 
      undefined. 

All extra arguments are available via the ``z_context:get_q/2`` function (and friends).

Calling API methods
^^^^^^^^^^^^^^^^^^^

A third way is to write your own API calls (see:
:ref:`manual-services`) and use standard jQuery to perform Ajax
GET/POST requests from the browser.

This use is perfectly possible and legal, although the other
methods are preferred, as they integrate nicely with the notification
and action systems. The API is more targeted to other applications
interfacing to Zotonic.


Using the transport functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The transport functions are the first layer above the websocket, comet and ajax methods
used for communication between the server and the browser.

There is an in-depth discussion with examples available, see :ref:`manual-transport`


PubSub using MQTT and pubzub.js
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It is possible to publish and subscribe to topics on the server.
Messages are relayed between the server and the browser.

See :ref:`mod_mqtt` for more information.

An example of MQTT PubSub usage is the custom tag :ref:`scomp-live`

