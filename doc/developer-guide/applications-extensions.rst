.. _guide-extensions:

Applications and extensions
===========================

.. _erlang-applications:

Erlang applications
-------------------

You can use Erlang applications in your Zotonic instance. This is useful when
you want to create a Zotonic module that integrates a (third-party) Erlang
application into Zotonic. Let’s take `mod_elasticsearch`_, which depends on the
`erlastic_search`_ library, as an example.

To use `Erlang applications`_ in your Zotonic setup, first include them under
:ref:`deps` in your ``zotonic.config`` file:

.. code-block:: erlang
    :caption: zotonic.config

    %% ...

    {deps, [
        {erlastic_search, "1.3.0", {git, "git://github.com/tsloughter/erlastic_search", {tag, "v1.3.0"}}}
    ]},

Then start the application in your module with
``application:ensure_all_started``:

.. code-block:: erlang
    :caption: modules/mod_elasticsearch/mod_elasticsearch.erl

    -export([
        init/1
        %% ...
    ]).

    init(Args) ->
        application:ensure_all_started(erlastic_search),
        %% ...

Configuration for the Erlang application can be added to your
:ref:`erlang.config <erlang-config>` file:

.. code-block:: erlang
    :caption: erlang.config

    %% ...

    {erlastic_search, [
        {host, <<"127.0.0.1">>}
    ]}

Extensions
----------

Zotonic has an extra mechanism for starting additional things that
need to be running under the ``zotonic_sup`` supervisor. These are
called `extensions`. Extensions are not tied to any particular Zotonic
site, but are regular Erlang processes, in OTP fashion.

On the startup of Zotonic, the global ``$ZOTONIC/priv/extensions`` folder is
scanned once for folders starting with ``ext_``. Each of these folders
is considered an `extension`.

An extension (for instance, ``ext_foo/ext_foo.erl``) can be a regular
Erlang module, but is supposed to be something supervisable like a
gen_server. It needs to expose at least a function
``start_link/0``. On Zotonic startup, this function is called and the
resulting pid is added to the Zotonic supervision tree as a `worker` process.


.. _Erlang applications: http://erlang.org/doc/apps/kernel/application.html
.. _mod_elasticsearch: https://github.com/driebit/mod_elasticsearch
.. _erlastic_search: https://github.com/tsloughter/erlastic_search
