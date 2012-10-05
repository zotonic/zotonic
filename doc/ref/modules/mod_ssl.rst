
.. include:: meta-mod_ssl.rst

The mod_ssl module adds https support. After enabling mod_ssl the logon window 
and other secure pages will be served using https.

SSL support can be switched on for every site separately. Though, because of the nature of 
SSL, every site will listen on its own port or IP address. Virtual hosting of sites via
https is not possible.


Configuration
-------------

There are three configurarion options. They can be set in the admin. All three are optional
and will be either set or replaced with a default when not set.

``mod_ssl.listen_port``
    This is the port mod_ssl will start the https listener on. When it is not defined then
    mod_ssl will assign a random port and set the ``mod_ssl.listen_port`` to that port number.

    Note that on Unix and BSD it is not possible to use ports below 1024. You need to
    map ports below 1024 to the ``mod_ssl.listen_port``.
    See ...
.. todo:: Add ref to "How to bind Zotonic to Port 80 and Port 443"

``mod_ssl.port``
    This is the *outside* port, as seen by a visitor of the site. When this is set to 443 then
    no port will be visible and the URL will look like ``https://example.com//``.

    This outside port **must** be mapped to the ``mod_ssl.listen_port``.

    When ``mod_ssl.port`` is not configured then the ``mod_ssl.listen_port`` will be used
    in the URLs. For example: ``https://example.com:54163/``

``mod_ssl.is_secure``
    When this configuration is set to something else then ``0`` or ``false`` then mod_ssl will
    ensure that sessions started while using https are secured and only valid on https.

    The *autologon* cookie will also be https only (if set from a https connection).

    Besides that mod_ssl will ensure that, after using https, all following pages will be
    served using https. Unless specifically specified otherwise in the dispatch rules.


SSL Certificates
----------------

For a https connection special encryption keys and certificates are needed. These are supplied
by many companies, for very different price ranges, usage and security levels.

There is an exception, when only a secure connection is needed and the only security is against
eaves dropping. This is by using a *self signed certificate*. This is a key and certificate
that is generated on the server, and does not guarantee anything about the validity of the
certificate.

When there is no certificate mod_ssl will generate a self signed certificate.


Certificate and key files
-------------------------

The files with the certificates and key are placed into the ``ssl`` directory inside the site
directory::

    zotonic/priv/sites/sitename/ssl/

Where ``sitename`` is the name of your site.

The files all have the name of the site in them (*sitename* in the filenames below).
This is to prevent mixing them up with other sites:

``sitename.pem``
    This holds the private key for the encryption. The key must be unlocked and in 
    PKCS#1 format (see below).

``sitename.crt``
    This is the certificate. Usually it is supplied by the certificate authority where you 
    bought it. It can also be a self signed certificate, see below.

``sitename.ca.crt``
    This is the *CA bundle* that contains root and intermediate certificate for
    the certificate authority that issued the ``sitename.crt`` certificate.

    The certificate authority will supply these. All supplied certificates are
    concatenated, with the root certificate last.

    The concatenation is a literal command, like::

        cat intermediate.crt root.crt > sitename.ca.crt

    This file should not be present when using a self signed certificate.


Dependencies
------------

When mod_ssl needs to generate or convert key and/or certificates it needs ``openssl``.
This program must be installed in the normal search path of the running Zotonic.


Format of the private key
-------------------------

The Erlang SSL implementation uses PKCS#1 format keys. OpenSSL generates (since 2010) PKCS#8
format keys. The difference can be seen when inspecting the key file. A PKCS#1 key starts with::

    -----BEGIN RSA PRIVATE KEY-----

Where a PKCS#8 key starts with::

    -----BEGIN PRIVATE KEY-----

When mod_ssl sees that the key file is a PKCS#8 file then it will stop and give the following
error::

    {error, {need_rsa_private_key, "example.pem", "use: openssl rsa -in sitename.key -out sitename.pem"}}

The given command is the command needed to convert the key to a PKCS#1 key. The
PKCS#8 key should be renamed to ``sitename.key`` from ``sitename.pem``, before running the
above command.

Note that the resulting key file *must* be named ``sitename.pem`` where ``sitename`` is the name of
the site the key is placed in.


Generating the self signed certificate
--------------------------------------

For generating the self signed certificate, mod_ssl runs the following commmands (where ``sitename`` should
be replaced with the name of the site)::

    openssl req -x509 -nodes -days 3650 -subj '/CN=www.example.com' -newkey rsa:2048 \
         -keyout sitename.key -out sitename.crt

This generates a private key of 2048 bits and a certificate that is valid for 10 years.

Optionally, when the key turns out to be in PKCS#8 format, mod_ssl will run the following command as well::

    openssl rsa -in sitename.key -out sitename.pem

When the key is already in PKCS#1 format (with older openssl installs) then mod_ssl will rename
the ``sitename.key`` file to ``sitename.pem``.


.. todo:: Work in progress.
