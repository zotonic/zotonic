.. _cookbook-custom-controller:

Create a custom controller
==========================

Zotonic comes with a large collection :ref:`controllers <guide-controllers>`
that cover many use cases, so you’ll probably have to resort to custom
controllers less often than you may be used to from other web frameworks. Still,
the time may come when you need to process HTTP requests in your own way.

You can do so by creating a custom :ref:`controller <guide-controllers>`. Create
a module in your site’s :file:`controllers/` directory and prefix it with
``controller_``:

.. code-block:: erlang
    :caption: yoursite/controllers/controller_say_hello.erl

    -module(controller_say_hello).

    -export([
        html/1
    ]).

    %% Include this to have a working controller out of the box
    -include_lib("controller_html_helper.hrl").

    %% This function renders some HTML when the controller is called
    html(Context) ->
        {<<"Hello world and all the people in it!">>, Context}.

To be able to handle HTTP requests with this controller, you need to
:ref:`define a dispatch rule <defining-dispatch-rules>` that maps some request
URL to this controller:

.. code-block:: erlang
    :caption: yoursite/dispatch/dispatch

    [
        {say_hello, ["hello"], controller_say_hello, []}
    ].

Now, if you go to ``http://yoursite.com/hello`` in your browser, you will see
the output of the ``controller_say_hello`` controller.

Handling POST requests
----------------------

Now you’ve seen how to handle GET requests, let’s turn to POST requests. First,
your controller must show that it can handle POSTs. You do so by adding an
``allowed_methods/2`` function:

.. code-block:: erlang
    :caption: yoursite/controllers/controller_say_hello.erl

    -module(controller_say_hello).

    -export([
        %% ...
        allowed_methods/2
    ]).

    %% ...

    %% This controller will handle only GETs and POSTs
    allowed_methods(ReqData, State) ->
        {['GET', 'POST'], ReqData, State}.

A ``process_post/2`` function will be called with the POST data, so define it:

.. code-block:: erlang
    :caption: yoursite/controllers/controller_say_hello.erl

    -module(controller_say_hello).

    -export([
        allowed_methods/2,
        html/1,
        process_post/2,
    ]).

    -include_lib("controller_html_helper.hrl").

    allowed_methods(ReqData, State) ->
        {['GET', 'POST'], ReqData, State}.

    html(Context) ->
        {<<"Hello world and all the people in it!">>, Context}.

    process_post(ReqData, Context) ->
        %% Process the POST data
        Context1 = ?WM_REQ(ReqData, Context),
        Context2 = z_context:ensure_qs(Context1),
        Name = list_to_binary(z_context:get_q(name, Context2)),

        {{halt, 200}, wrq:set_resp_body(<<"Thank you posting, ", Name/binary>>, ReqData), Context}.

Try it out on the command line:

.. code-block:: shell

    $ curl -v -X POST -d 'name=David' http://yoursite.test/hello

    # prints:
    Thank you posting, David

.. seealso::

    * :ref:`guide-controllers` in the Developer Guide
