
.. include:: meta-mod_ssl_self_signed.rst

The mod_ssl_self_signed module adds a basic certificate handling functionality
to your Zotonic sites. When this module is enabled and there are no certificates
or keys available it generates new keys and self signed certificates to get started
easily.


SSL Certificates
----------------

For a HTTPS connection special encryption keys and certificates are needed. These are supplied
by many companies, for very different price ranges, usage and security levels.

There is an exception, when only a secure connection is needed and the only security is against
eaves dropping. This is by using a *self signed certificate*. This is a key and certificate
that is generated on the server, and does not guarantee anything about the validity of the
certificate.

When there is no certificate mod_ssl will generate a self signed certificate.

Certificate and key files
-------------------------

The files with the certificates and key are placed into the ``ssl`` directory inside the site
directory :file:`user/sites/sitename/ssl/`.

Where *sitename* must be replaced with the name of your site.

The files all have the name of the site in them (*sitename* in the filenames below).
This is to prevent mixing them up with other sites:

:file:`sitename.pem`
    This holds the private key for the encryption. The key must be unlocked and in
    PKCS#1 format (see below).

:file:`sitename.crt`
    This is the certificate. Usually it is supplied by the certificate authority where you
    bought it. It can also be a self signed certificate, see below.

:file:`sitename.ca.crt`
    This is the *CA bundle* that contains root and intermediate certificates for
    the certificate authority that issued the :file:`sitename.crt` certificate.

    The certificate authority will supply these. All supplied certificates are
    concatenated, with the root certificate last.

    The concatenation is a literal command, like::

        cat intermediate.crt root.crt > sitename.ca.crt

    This file should not be present when using a self signed certificate.


Dependencies
------------

When mod_ssl needs to generate or convert key and/or certificates it needs :program:`openssl`.
This program must be installed in the normal search path of the running Zotonic.


Format of the private key
-------------------------

The Erlang SSL implementation uses PKCS#1 format keys. OpenSSL generates (since 2010) PKCS#8
format keys. The difference can be seen when inspecting the key file. A PKCS#1 key starts with:

.. code-block:: none

    -----BEGIN RSA PRIVATE KEY-----

Where a PKCS#8 key starts with:

.. code-block:: none

    -----BEGIN PRIVATE KEY-----

When mod_ssl sees that the key file is a PKCS#8 file then it will stop and give the following
error::

    {error, {need_rsa_private_key, "example.pem", "use: openssl rsa -in sitename.key -out sitename.pem"}}

The given command is the command needed to convert the key to a PKCS#1 key. The
PKCS#8 key should be renamed to :file:`sitename.key` from :file:`sitename.pem`, before running the
above command.

Note that the resulting key file *must* be named :file:`sitename.pem` where *sitename* is the name of
the site the key is placed in.


Using SSL certificates
----------------------

If you order a SSL certificate, the signing authority will ask you which kind of web server you are using and a CSR file.
For the web server, select *other*. For the CSR, use the following command (replace ``sitename`` with
the name of your site)::

    openssl req -out sitename.csr -new -newkey rsa:2048 -nodes -keyout sitename.key

When OpenSSL asks for the *Common Name* then fill in the site’s hostname (e.g. *www.example.com*).

The resulting ``.key`` file can be converted to a ``.pem`` file::

    openssl rsa -in sitename.key -out sitename.pem

From the SSL certificate authority you will receive a signed ``.crt`` file.

See the section *Certificate and key files* above for instructions how to use the ``.crt`` and ``.pem`` files.


Generating the self signed certificate
--------------------------------------

For generating the self signed certificate, mod_ssl runs the following commmands (where ``sitename`` should
be replaced with the name of the site):

.. code-block:: bash

    openssl req -x509 -nodes -days 3650 -subj '/CN=www.example.com' -newkey rsa:2048 \
         -keyout sitename.key -out sitename.crt

This generates a private key of 2048 bits and a certificate that is valid for 10 years.

Optionally, when the key turns out to be in PKCS#8 format, mod_ssl will run the following command as well:

.. code-block:: bash

    openssl rsa -in sitename.key -out sitename.pem

When the key is already in PKCS#1 format (with older openssl installs) then mod_ssl will rename
the :file:`sitename.key` file to :file:`sitename.pem`.


.. todo:: Add SSL/certificate problem solving.

