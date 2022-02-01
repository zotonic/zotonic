.. _cookbook-logstash:

Logging to Logstash
===================

.. seealso:: the :ref:`dev-logging` chapter in the Developer Guide.

`Logstash`_ is often used for log centralization and analysis. This cookbook
describes how to set up Zotonic for logging to Logstash over UDP. As mentioned
in the :ref:`Logging chapter <dev-logging>`, Zotonic uses `Logger`_.

So we will change Zotonic’s Logstash configuration in order
to send messages to Logstash.

Step 1: add a Logstash handler
------------------------------

Zotonic comes with the `logstash handler`_ ``logstasher_h`` for logger. The
handler will be started automatically if it is configured as a logger
handler in :ref:`erlang-config`:

.. code-block:: erlang
    :caption: erlang.config

    {kernel, [
        % Minimum log level for all loggers below.
        {logger_level, info},

        {logger, [

            %% To use logstash:
            %% - Enable the logstasher_h handler
            %% - Configure logstasher (see below the kernel config)
            %%
            {handler, logstash, logstasher_h,
                #{
                    level => info
                }
            },

            %%% Other logger configs here
            ...
        }
    ]},


Step 2: configure the Logstash handler
--------------------------------------

The next step is to tell the Logstash handler where it should send its messages
to. The configuration of ``logstasher`` in the :ref:`erlang-config` can be found
below the kernel section or else added there:

.. code-block:: erlang
    :caption: erlang.config

    %% Logstash configuration.
    %% If a logger handler with 'logstasher_h' is defined then zotonic_core will start the
    %% logstasher application.
    {logstasher, [
        {transport, udp},     % tcp | udp
        {host, "localhost"},  % inet:hostname()
        {port, 5000}          % inet:port_number()
    ]},


Replace ``logger`` with the hostname or IP address of your logger. IP addresses can
be configured as an Erlang tuple, for example: ``{127,0,0,1}``

After you changed the ``erlang.config`` file you will need to restart Zotonic.

You should now find all Zotonic log messages in Logstash. To test this, just
call:

.. code-block:: erlang

    logger:error_msg("Just testing the Logstash setup here!").


.. _Logstash: http://www.elastic.co/products/logstash
.. _Logger: https://www.erlang.org/doc/apps/kernel/logger_chapter.html
.. _logstash handler: https://github.com/zotonic/logstasher

