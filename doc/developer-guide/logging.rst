.. _dev-logging:

Logging
=======

.. seealso:: :ref:`cookbook-logstash` cookbook
.. seealso:: To log messages to the database, you can use :ref:`mod_logging`.

Zotonic uses `Logger`_ for logging. Lager metadata is automatically set by
Zotonic, so to log a message from your code, simply call Lager directly:

.. code-block:: erlang

    lager:error("Something went very wrong with ~p", [SomeParam]).

which will write the following in the :file:`error.log` file:

.. code-block:: none

    2022-01-28 17:17:45 ERROR <0.9.0> [some_module:some_function/0:42] ▸ text="Something went very wrong with whatever"


Configuration
-------------

In :ref:`erlang-config` file, change the ``lager`` section to configure log
handlers and formatters. See the `Logger documentation`_ for more information.

For instance, to configure Logger for logging to Logstash, include a Logstash
handler as a dependency in your project, and add something like this:

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

.. _Logger: https://www.erlang.org/doc/man/logger.html
.. _Logger documentation: https://www.erlang.org/doc/apps/kernel/logger_chapter.html
