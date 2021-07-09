.. highlight:: erlang

.. _ref-port-ssl-configuration:

Port configurations
-------------------

Port configurations can be tricky, especially in combination with SSL.
Here we explain all steps to come to a correctly configured installation
with working SSL connectivity.

There are basically two sets of port configurations:

 1. The ports Zotonic *listens* on
 2. The ports an *outside* visitor *connects to*

The listen ports are configured with ``listen_port`` and ``ssl_listen_port``.

The outside ports are configured with ``port`` and ``ssl_port``. They default to
the *listen_* variations if not defined.

Below are examples how to configure these.


Server direct on the Internet
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Here you can use the methods described in :ref:`guide-deployment-privilegedports` to get your server
on ports 80 and/or 443.

The port configurations would be:

+---------------+------------+-----------------+------+------------+-----------+-----------------+
|Method         |listen_port | ssl_listen_port | port | ssl_port   | listen_ip | proxy_allowlist |
+===============+============+=================+======+============+===========+=================+
|authbind       |80          | 443             | 80   | 443        | any       | none            |
+---------------+------------+-----------------+------+------------+-----------+-----------------+
|setcap         |80          | 443             | 80   | 443        | any       | none            |
+---------------+------------+-----------------+------+------------+-----------+-----------------+
|iptables       |8000        | 8443            | 80   | 443        | 127.0.0.1 | none            |
+---------------+------------+-----------------+------+------------+-----------+-----------------+
|http only      |8000        | none            | 80   | none       | 127.0.0.1 | none            |
+---------------+------------+-----------------+------+------------+-----------+-----------------+

For *Network Address Translation* (NAT), see the next section. The *proxy_allowlist* is explained
in the section about proxies below.

In the case of *iptables* we restrict Zotonic to listen on the local 127.0.0.1 address.
This to prevent that people can connect on port 8000 from their browsers.

Alternatively you can run your server on the *outside* port 8000, though then it is impossible
to use Let’s Encrypt certificates for SSL (as they require the server to run on the default
http and https ports).

Server accessed via NAT
^^^^^^^^^^^^^^^^^^^^^^^

With *Network Address Translation* (NAT) the traffic is routed straight to the server using port
mappings. This is typical for a situation where Zotonic runs on a local server behind a modem.

+---------------+------------+-----------------+------+------------+-----------+-----------------+
|Proxy method   |listen_port | ssl_listen_port | port | ssl_port   | listen_ip | proxy_allowlist |
+===============+============+=================+======+============+===========+=================+
|NAT (eg. modem)|8000        | 8443            | 80   | 443        | any       | none            |
+---------------+------------+-----------------+------+------------+-----------+-----------------+

The *proxy_allowlist* is explained in the section about proxies below.

Server behind a proxy like Nginx or HAProxy
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A proxy could be *haproxy* or *nginx*. The proxy terminates the https connection and handles
the SSL certificates.

Typically the proxy connects to the default ports 8000 and 8443 on the Zotonic server.
The proxy itself could be running on the local server or another server.

+---------------+------------+-----------------+------+------------+-----------+-----------------+
|Proxy method   |listen_port | ssl_listen_port | port | ssl_port   | listen_ip | proxy_allowlist |
+===============+============+=================+======+============+===========+=================+
|localhost proxy|8000        | none            | 80   | 443        | 127.0.0.1 | local           |
+---------------+------------+-----------------+------+------------+-----------+-----------------+
|proxy on LAN   |8000        | none            | 80   | 443        | any       | local           |
+---------------+------------+-----------------+------+------------+-----------+-----------------+
|proxy on WAN   |8000        | none            | 80   | 443        | any       | *see below*     |
+---------------+------------+-----------------+------+------------+-----------+-----------------+

The proxy adds the hostname, address of the visitor and protocol information (http or https) to a
HTTP header. Zotonic reads this header to know which site to serve and if the visitor was using https
or not.

Everybody could add this header and then connect directly to the Zotonic server, which can then make
wrong assumptions about the IP address of the visitor and if the visitor is on a secure connection.

To prevent the visitor spoofing the *Forward* header, Zotonic will check if the *inside* address of the
proxy (as seen from Zotonic, not from the visitor) is on a list of allowed proxies.

This allowlist is specified in ``proxy_allowlist`` and can have the following values:

 * ``local`` - default, only LAN addresses can connect
 * ``none`` - no proxy exists, ignore proxy headers
 * ``any`` - *insecure*, any server anywhere can be a proxy
 * A tuple with a single ip address, for example: ``{192,168,1,1}``
 * A string with ip addresses with optional masks, for example: ``"127.0.0.0/8,10.0.0.0/8,fe80::/10"``

The ``local`` check is hardcoded and very fast.

SSL certificates
^^^^^^^^^^^^^^^^

After the server’s listen ports are correctly configured then the SSL connection can be tested.

Per default Zotonic will generate a self-signed certificate for all valid hostnames. Instead of these
self-signed certificates a real certificate can be used. Check for these the modules :ref:`mod_ssl_letsencrypt` and
:ref:`mod_ssl_ca`

The self-signed certificates are stored in the :file:`self-signed` subdirectory of the security directory.

You can see the ``security_dir`` location using ``bin/zotonic config``

Possible locations are:

 * The environment variable ``ZOTONIC_SECURITY_DIR``
 * The :file:`~/.zotonic/security` directory
 * The :file:`/etc/zotonic/security` directory (only on Linux)
 * The OS specific directory for application data files

The OS specific directories are:

 * On Unix: :file:`~/.config/zotonic/security/`
 * On macOS: :file:`~/Library/Application Support/zotonic/security/`

The default is the OS specific directory.


HTTPS and security
^^^^^^^^^^^^^^^^^^

Zotonic always redirects all incoming HTTP connections to HTTPS. This can not be disabled.


Secure cookies
""""""""""""""

All cookies related to authentication are set to *secure*. The cookie holding the authentication
token has ``SameSite`` set to ``Strict``. This can not be configured or changed.


Erlang SSL Configuration
^^^^^^^^^^^^^^^^^^^^^^^^

The erlang ssl application is configured in the :file:`erlang.config` file. It is stored next to the
``zotonic.config`` file. You can see the config files used with::

  bin/zotonic configfiles

If this file is missing then it can be copied from :file:`apps/zotonic_launcher/priv/erlang.config.in`.
It contains a couple of important settings which we recommend you to change. The reason for this is that the
default settings Erlang uses are unsuitable for web servers. The most important settings are listed
below.

``session_lifetime``
  Sets the maximum lifetime of session data in seconds.

``session_cache_server_max``
  Sets the maximum number of client sessions cached by the server.

For more information on configuration options, please see `Erlang SSL App`_.


.. _Erlang SSL: http://erlang.org/doc/man/ssl.html
.. _Erlang SSL App: http://erlang.org/doc/man/ssl_app.html


Adding your own SSL options or certificates
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you want to implement your own certificate handling you have to add a
notification observer which returns the certificates to the underlying
HTTPS server. This can be needed if you have a site with special hostname aliases, or if
you want to implement automated certificate handling for a specific certificate authority.

The notification use by the SNI (Server Name Indication) handler is:

``ssl_options{server_name=ServerName}``
  Return the certificate, key or other ssl options. ``ServerName`` is a string (list) with the
  name of the server from the SSL handshake. You shoudl return a proplist with Erlang
  ``ssl:ssl_option()`` terms. The proplist will override the default ssl options for this
  connection. For more information about the possible properties see `Erlang SSL`_.
  If ``undefined`` is returned the SSL handshake will try the next SSL module. If all
  modules return ``undefined`` then a self-signed certificate will be used.


.. seealso:: :ref:`mod_ssl_letsencrypt`, :ref:`mod_ssl_ca`, :ref:`guide-deployment-privilegedports`
