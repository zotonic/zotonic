.. _cookbook-custom-controller:

Create a custom controller
==========================

Zotonic comes with a large collection :ref:`controllers <guide-controllers>`
that cover many use cases, so you’ll probably have to resort to custom
controllers less often than you may be used to from other web frameworks. Still,
the time may come when you need to process HTTP requests in your own way.

You can do so by creating a custom :ref:`controller <guide-controllers>`. Create
a module in your site’s :file:`src/controllers/` directory and prefix it with
``controller_``:

.. code-block:: erlang
    :caption: yoursite/src/controllers/controller_say_hello.erl

    -module(controller_say_hello).

    -export([
        process/4
    ]).

    %% This function renders some HTML when the controller is called
    process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
        {<<"Hello world and all the people in it!">>, Context}.

To be able to handle HTTP requests with this controller, you need to
:ref:`define a dispatch rule <defining-dispatch-rules>` that maps some request
URL to this controller:

.. code-block:: erlang
    :caption: yoursite/priv/dispatch/dispatch

    [
        {say_hello, ["hello"], controller_say_hello, []}
    ].

Now, if you go to ``https://yoursite.test:8443/hello`` in your browser, you will see
the output of the ``controller_say_hello`` controller.

Handling POST requests
----------------------

Now you’ve seen how to handle GET requests, let’s turn to POST requests. First,
your controller must show that it can handle POSTs. You do so by adding an
``allowed_methods/1`` function:

.. code-block:: erlang
    :caption: yoursite/src/controllers/controller_say_hello.erl

    -module(controller_say_hello).

    -export([
        %% ...
        allowed_methods/1
    ]).

    %% ...

    %% This controller will handle only GETs and POSTs
    allowed_methods(Context) ->
        {[<<"GET">>, <<"POST">>], Context}.

The ``process/4`` function will be called with the POST data, so define it:

.. code-block:: erlang
    :caption: yoursite/src/controllers/controller_say_hello.erl

    -module(controller_say_hello).

    -export([
        allowed_methods/1,
        process/4,
    ]).

    allowed_methods(Context) ->
        {[<<"GET">>, <<"POST">>], Context}.

    process(<<"GET">>, _AcceptedCT, _ProvidedCT, Context) ->
        {<<"Hello world and all the people in it!">>, Context};
    process(<<"POST">>, _AcceptedCT, _ProvidedCT, Context) ->
        % Process the POST data
        Name = z_html:escape( z_context:get_q(<<"name">>, Context, <<>>) ),
        {<<"Thank you posting, ", Name/binary>>, Context}.

Try it out on the command line:

.. code-block:: shell

    $ curl -k -v -X POST -d 'name=David' https://yoursite.test:8443/hello

    # prints:
    Thank you posting, David

.. seealso::

    * :ref:`guide-controllers` in the Developer Guide
