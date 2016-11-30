.. _dev-logging:

Logging
=======

Zotonic uses `Lager`_ for logging. Lager metadata is automatically set by
Zotonic, so to log a message from your code, simply call Lager directly::

    lager:error("Something went very wrong with ~p", [SomeParam]).

which will write the following in the :file:`error.log` file:

.. code-block:: none

    2016-11-15 16:34:07.629 [error] yoursite <0.890.0>@some_module:some_function:42 Something went very wrong with whatever

Configuration
-------------

In :ref:`erlang-config` file, change the ``lager`` section to configure log
handlers and formatters. See the `Lager documentation`_ for more information.

For instance, to configure Lager for logging to Logstash, include a Logstash
handler as a :ref:`dependency <deps>` in your project, and add something like
this:

.. code-block:: erlang
    :caption: erlang.config

    {lager, [
        {handlers, [
            {lager_console_backend, info},
            {lager_logstash_backend, [
                {level, info},
                {output, {udp, "logs.yourcompany.com", 5514}},
                {format, json},
                {json_encoder, jsx}
            ]}
        ]},
        {crash_log, "priv/log/crash.log"}
    ]},

.. _Lager: https://github.com/erlang-lager/lager
.. _Lager documentation: https://github.com/erlang-lager/lager#configuration

.. seealso::

    * :ref:`cookbook-logstash` cookbook
    * To log messages to the database, you can use :ref:`mod_logging`.
