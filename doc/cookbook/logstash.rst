.. _cookbook-logstash:

Logging to Logstash
===================

`Logstash`_ is often used for log centralization and analysis. This cookbook
describes how to set up Zotonic for logging to Logstash over UDP. As mentioned
in the :ref:`Logging chapter <dev-logging>`, Zotonic uses the
`Lager framework`_. So we will change Zotonic’s Logstash configuration in order
to send messages to Logstash.

Step 1: install a Logstash handler
----------------------------------

First, you need to install a Logstash handler for Lager, for instance
`this one`_. To do so, add it (and the jsx JSON library) as a
:ref:`dependency <deps>` in the ``rebar.config`` of your Zotonic site or module:

.. code-block:: erlang
    :caption: rebar.config

    {deps, [
        {lager_logstash, {git, "https://github.com/rpt/lager_logstash.git", {tag, "0.1.3"}}},
        {jsx, {git, "https://github.com/talentdeficit/jsx.git", {tag, "2.8.0"}}}
    ]}

And recompile Zotonic to install the dependencies.

Step 2: configure the Logstash handler
--------------------------------------

The next step is to tell the Logstash handler where it should send its messages
to:

.. code-block:: erlang
    :caption: erlang.config

    {lager, [
        {handlers, [
            %% Keep the console backend
            {lager_console_backend, info},

            %% And add a Logstash backend
            {lager_logstash_backend, [
                {level, info},

                %% Change the host and port to your Logstash server
                {output, {udp, "logs.yourcompany.com", 5514}},
                {format, json},
                {json_encoder, jsx}
            ]}
        ]},
        {crash_log, "priv/log/crash.log"}
    ]},

Step 3: Logstash configuration
------------------------------

You need to configure your Logstash server so it can receive messages from
Zotonic. A simple configuration should suffice:

.. code-block:: none
    :caption: logstash.conf

    input {
        udp {
            port => 5514
            codec => "json"
        }
    }

    output {
        elasticsearch {
            hosts => ["your_elastic:9200"]
        }
    }

You should now find all Zotonic log messages in Logstash. To test this, just
call::

    lager:error("Just testing the Logstash setup here!").

.. seealso:: the :ref:`dev-logging` chapter in the Developer Guide.

.. _Logstash: http://www.elastic.co/products/logstash
.. _Lager framework: https://github.com/erlang-lager/lager
.. _this one: https://github.com/rpt/lager_logstash

