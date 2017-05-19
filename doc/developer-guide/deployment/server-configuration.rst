.. highlight:: bash

.. _guide-deployment-server-configuration:

Server configuration
====================

This chapter describes how to configure your Linux server for running Zotonic.

.. _file-descriptors:

File descriptors
----------------

By default, Linux limits the number of file descriptors (i.e. the maximum number
of open files) at 1024. Running Zotonic at scale will require many more than
that, particularly because Zotonic uses
:ref:`WebSockets <controller-websocket>` extensively; remember that every open
port is an open file.

The limit applies on several levels:

1. the process, in our case the Erlang VM (BEAM) that runs Zotonic
2. the Zotonic user
3. system-wide.

To check the current process usage, find BEAM’s PID::

    # pidof beam.smp
    10006 (for instance)

Then count its open files::

    # lsof -a -p 10006 | wc -l

And compare it to the process limit::

    # cat /proc/10006/limits | grep 'Max open files'

You can raise the process limit by adding ``ulimit -n`` to your Zotonic init
script::

    ulimit -n 50000

Or change the limit in your system definition. For instance, when using
systemd::

    [Service]
    LimitNOFILE=50000

Finally, make sure to check `your system-wide limits`_, too.

Network connections
-------------------

If you have stateful connection tracking enabled, high-traffic Zotonic sites
may `overflow your conntrack table`_.

Compare the current number of connections with the limit::

    # sysctl net.netfilter.nf_conntrack_max
    # sysctl net.netfilter.nf_conntrack_count

When you increase the maximum number of connections in the connection tracking
table it is important to increase the size of the table which stores the 
connections. 
The rule of the thumb for this is: ``nf_conntrack_buckets = nf_conntrack_max / 8``.

If you need to raise the limit, edit ``/etc/sysctl.conf``:

.. code-block:: none
    :caption: /etc/sysctl.conf

    net.netfilter.nf_conntrack_max = some_number
    net.netfilter.nf_conntrack_buckets = some_other_number

Moreover, to reduce the number of open connections, you can decrease their
time-out values so they get closed sooner. By default, inactive connections can
stay in the conntrack table for 5(!) days. To change this:

.. code-block:: none
    :caption: /etc/sysctl.conf

    net.netfilter.nf_conntrack_tcp_timeout_established = 600

If you have a proxy in front of Zotonic (e.g. HAProxy or Varnish), you need to
change the limits on the proxy, too.

.. seealso::

    * See :ref:`cookbook-exometer` on how to monitor your Zotonic system.

    * For general Erlang troubleshooting, Fred Hebert’s free ebook
      `Stuff goes bad: Erlang in anger`_ is a very good resource.

.. _`Stuff goes bad: Erlang in anger`: https://www.erlang-in-anger.com
.. _overflow your conntrack table: http://antmeetspenguin.blogspot.nl/2011/01/high-performance-linux-router.html
.. _your system-wide limits: https://www.cyberciti.biz/faq/linux-increase-the-maximum-number-of-open-files/
