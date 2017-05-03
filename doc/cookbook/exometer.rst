.. _cookbook-exometer:

Exometer metrics
================

Zotonic comes with a system for collecting and exporting metrics (such as how much memory is used, how many database requests were made, etc.) called Exometer. This cookbook details how to make use of that system to report on your Zotonic server's activity.

Step 1: get a Time Series Database (TSDB)
-----------------------------------------

At the time of writing, Exometer in Zotonic supports sending metrics to Graphite, OpenTSDB and StatsD, as well as being able to send them to another system using AMQP or polling through SNMP. The installation of such a system is outside the scope of this document, but if you want to experiment without setting up a TSDB for yourself, `Hosted Graphite`_ has a 14-day free trial to play with.

Step 2: configure Exometer to report to your TSDB
-------------------------------------------------

After deciding on a TSDB, you need to configure Exometer to connect to the TSDB. This is done by configuring a *reporter* in your erlang.config. 

The default erlang.config comes with a number of pre-defined exometer metrics:

.. code-block:: erlang

    {exometer, [{predefined, [
        {[erlang, memory], {function, erlang, memory, [], value, []}, []},
        {[erlang, system_info], {function, erlang, system_info, ['$dp'], value, [process_count]}, []},
        {[erlang, statistics], {function, erlang, statistics, ['$dp'], value, [run_queue]}, []},
        {[erlang, io], {function, erlang, statistics, [io], match, {{'_', input}, {'_', output}}}, []}
       ]}
    ]},

To configure a reporter, add a block called {reporters, []} and a block inside that for the reporter. For example, to report to a graphite server, the following configuration could be used:

.. code-block:: erlang

    {reporters, [
        {exometer_report_graphite, [
            {connect_timeout, 5000},
            {prefix, "zotonic"},
            {host, "graphite.server.com"},
            {api_key, ""},
            {port, 2003}
        ]}
    ]}

For testing, it is also possible to simply send the output to the console. For this, use the ``exometer_report_tty`` reporter, like so:

.. code-block:: erlang

    {reporters, [
        {exometer_report_tty, []}
    ]}

Other reporters will require different configuration. For more examples, see the `Exometer documentation`_. It is possible to configure multiple reporters at the same time; you can then select what metrics are sent to what reporter with subscriptions.

Step 3: tell Exometer what to report, and how often
---------------------------------------------------

Now that Exometer knows *where* to send our metrics, we need to tell it *what* to send, and when to do it. This is done with a *subscription* and, like reporters, can be configured in erlang.config in a block called {subscribers, []}. The general format of a subscription is::

{reporter, metric, datapoint, interval}

Metrics have a hierarchical name consisting of a list of terms plus a datapoint name. For example, the number of requests made to Zotonic's status page is named::

[zotonic, zotonic_status, webzmachine, requests]

and the datapoint name is 'value'. So to report this metric to graphite every minute, we would add a subscription as follows:

.. code-block:: erlang

    {subscribers, [
        {exometer_report_graphite, [zotonic, zotonic_status, webzmachine, requests], value, 60000}
    ]}

Since the metric name includes the site name, it may be preferable to set up generic subscriptions instead, that would apply to all sites in the system. This can be done by using a special syntax in place of the metric name::

{select, [{matchspec}]}

Where ``matchspec`` is an `ETS Match Specification`_. The specifics of the Match specification can be quite complex, but we can use simple ones to get going. To select all zotonic metrics of type counter and send them to graphite every minute, you would enter the following::

{exometer_report_graphite, {select, [{ {[zotonic | '_'], counter, '_'}, [], ['$_'] }]}, value, 60000}

To send only metrics for a specific site, you can add the site name::

{exometer_report_graphite, {select, [{ {[zotonic, sitename | '_'], counter, '_'}, [], ['$_'] }]}, value, 60000}

Using the Zotonic Shell
-----------------------

It is possible to set up exometer from the Zotonic shell. As this run-time configuration is lost upon restart, it is best to only use this for testing or to enact configuration file changes without a restart. A number of uses for the shell are shown below.

.. highlight:: none

To test a match specification. Take the matchspec part of the metric name and feed it to exometer:select(). For example:: 

    (zotonic001@server)2> exometer:select([{ {[zotonic, zotonic_status | '_'], counter, '_'}, [], ['$_'] }]).
    [{[zotonic,zotonic_status,db,requests],counter,enabled}, …


To find out what datapoints a metric contains, you can ask for its information::

    (zotonic001@server)2> exometer:info([zotonic,zotonic_status,webzmachine,requests], datapoints).          
    [value,ms_since_reset]
    
To test a subscription, you can manually add it::

    (zotonic001@server)2> exometer_report:subscribe(exometer_report_tty, {select, [{ {[zotonic, zotonic_status | '_'], counter, '_'}, [], ['$_'] }]}, value, 60000).
    ok

.. highlight:: erlang

Complete configuration example
------------------------------

A complete configuration might look like the following:

.. code-block:: erlang

    {exometer, [
        {predefined, [
            {[erlang, memory], {function, erlang, memory, [], value, []}, []},
            {[erlang, system_info], {function, erlang, system_info, ['$dp'], value, [process_count]}, []},
            {[erlang, statistics], {function, erlang, statistics, ['$dp'], value, [run_queue]}, []},
            {[erlang, io], {function, erlang, statistics, [io], match, {{'_', input}, {'_', output}}}, []}
        ]},
        {reporters, [
            {exometer_report_graphite, [
                {connect_timeout, 5000},
                {prefix, "zotonic"},
                {host, "graphite.server.com"},
                {api_key, ""},
                {port, 2003}
            ]}
        ]},
        {subscribers, [
            {exometer_report_graphite, {select, [{ {[zotonic | '_'], counter, '_'}, [], ['$_'] }]}, value, 60000},
            {exometer_report_graphite, {select, [{ {[zotonic | '_'], gauge, '_'}, [], ['$_'] }]}, value, 60000},
            {exometer_report_graphite, {select, [{ {[erlang, memory], '_', '_'}, [], ['$_'] }]}, value, 60000},
        ]}
    ]},


.. _Exometer Documentation: https://github.com/Feuerlabs/exometer/tree/master/doc 
.. _ETS Match Specification: http://erlang.org/doc/apps/erts/match_spec.html
.. _Hosted Graphite: https://www.hostedgraphite.com/
