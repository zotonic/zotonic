
.. include:: meta-websocket.rst

Controller which accepts a WebSocket connection from the browser.

The controller provides persistent WebSocket connections between the client and the server. 
A WebSocket connection is automatically started on the page (unless nostream is given in the script tag).

See :ref:`guide-transport` for more information about transporting data between the server and the browser.

Defining Custom Websocket Behaviour
-----------------------------------

You can provide your own websocket_start similar too controller websocket by setting a ws_handler
containing the name of a websocket handler module in the zotonic context.

Example::

    websocket_start(Context) ->
        Context1 = z_context:set(ws_handler, ?MODULE, Context),
        controller_websocket:websocket_start(Context).

When passing a custom handler module, the default handler websocket will not be used, but the specified
one. Controller websocket contains the code for the default zotonic handler. It attaches itself as
websocket handler to the page session.

It is also possible to configure a custom ``ws_handler`` by specifying it in a dispatch rule.::

    {customws, ["socket", "custom"], controller_websocket, [{ws_handler, my_ws_handler}]}


.. highlight:: erlang

WebSocket Handler API
---------------------

In order to implement your own websocket handler you have to implement four callback functions.
When you want to sent a message to the client you call ``controller_websocket:send_data/2``.

Example::

  -module(my_ws_handler).

  -export([websocket_init/1,
           websocket_handle/2,
           websocket_info/2]).

  websocket_init(Context) ->
      PrunedContext = z_context:prune_for_scomp(Context),
      {ok, PrunedContext}.

  %% @doc Handle a message from the browser.
  websocket_handle({text, Data}, Context) ->
      {reply, {text, ["You said: ", Data]}, Context};
  websocket_handle({reply, Payload}, Context) ->
      % Pass the reply data from controller_websocket:send_data/2
      {reply, Payload, Context};
  websocket_handle(_Data, Context) ->
      {ok, Context}.

  websocket_info(_Msg, Context) ->
      controller_websocket:websocket_send_data(self(), <<"Hello from info/2">>}),
      {ok, Context}.

The websocket_init, websocket_handle and websocket_info callbacks
are called from within the controller’s receive loop, so to send a message
to the websocket, you send it to ``self()``, as in the example above or return
with ``{reply, {text, Text}, Context}``.
