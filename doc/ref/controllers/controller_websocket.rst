
.. include:: meta-websocket.rst

Controller which opens a WebSocket connection to the browser.

The controller provides persistent WebSocket connections between the
client and the server. The default implementation is used by :ref:`mod_base`
when the :ref:`{% stream %} <scomp-stream>` tag is placed on a page. 

Defining Custom Websocket Behaviour
-----------------------------------

You can provide your own websocket_start similar too controller websocket by setting a ws_handler 
containing the name of a websocket handler module in the zotonic context.

Example::

    websocket_start(ReqData, Context) ->
        Context1 = z_context:set(ws_handler, ?MODULE, Context),
        controller_websocket:websocket_start(ReqData, Context).

When passing a custom handler module, the default handler websocket will not be used, but the specified 
one. Controller websocket contains the code for the default zotonic handler. It attaches itself as 
websocket handler to the page session.

It is also possible to configure a custom ``ws_handler`` by specifying it in a dispatch rule.::

    {customws, ["socket", "custom"], controller_websocket, [{ws_handler, my_ws_handler}]}


WebSocket Handler without Zotonic session
-----------------------------------------

By default, ``contorller_websocket`` requires the ``z_pid`` and ``z_sid`` cookies to be sent to the WebSocket request, so that the controller will verify that the calling party has already a running session on the Zotonic instance. It is possible to override this behaviour to not require a session to be present. This is done through the ``require_session`` dispatch option, which defaults to ``true``.

To create a WebSocket controller without the caller needing to have a
session running, create a dispatch rule like this::

  {customws, ["socket", "without-session"], controller_websocket, [{require_session, false}]}
  


.. highlight:: erlang
               
WebSocket Handler API
---------------------

In order to implement your own websocket handler you have to implement four callback functions. 
When you want to sent a message to the client you call ``controller_websocket:send_data/2``.

Example::

  -module(my_ws_handler).

  -export([websocket_init/1,
           websocket_message/3,
           websocket_info/2,
           websocket_terminate/2]).
  
  %% @doc Called when the websocket is initialized.
  websocket_init(_Context) ->
      erlang:send_after(1000, self(), <<"Hello!">>),
      ok.

  %% @doc Called when a message arrives on the websocket.
  websocket_message(Msg, From, Context) ->
      controller_websocket:websocket_send_data(From, ["You said: ", Msg]).

  %% @doc Called when another type of message arrives.
  websocket_info(Msg, _Context) ->
      controller_websocket:websocket_send_data(self(), Msg),
      erlang:send_after(5000, self(), <<"Hello again!">>).

  %% @doc Called when the websocket terminates.
  websocket_terminate(Reason, Context) ->
      ok.

The websocket_init, websocket_info and websocket_terminate callbacks
are called from within the controller’s receive loop, so to send a message
to the websocket, you send it to ``self()``, as in the example above.

The `websocket_message` function however gets a `From` argument passed
to it because it is called from another process. To send a message to
the socket, you need to send it to the `From` pid.
