
.. include:: meta-mod_ssl_ca.rst

The mod_ssl_ca module adds support for using SSL certificates bought from a Certificate Authority.

A free alternative to CA provided tickets is Let’s Encrypt, see :ref:`mod_ssl_letsencrypt`.

Certificate and key files
-------------------------

The certificate and key files are placed into the site sub-directory of the security
directory. The subdirectory will be: ``sitename/ca/``

Where *sitename* must be replaced with the name of your site.

The security directory can be found by inspecting the output of::

  bin/zotonic config

The Zotonic *security* directory can be in one of the following directories:

 * The environment variable ``ZOTONIC_SECURITY_DIR``
 * The :file:`~/.zotonic/security` directory
 * The :file:`/etc/zotonic/security` directory (only on Linux)
 * The OS specific directory for application data files

The OS specific directories are:

 * On Unix: :file:`~/.config/zotonic/security/`
 * On macOS: :file:`~/Library/Application Support/zotonic/security/`

The default is the OS specific directory.

If there is a directory ``priv/security/ca`` inside your site's OTP application folder then
that directory will be used.

The filenames are checked against their extension. When you copy your files to the
``ca`` directory then you need to ensure that they have the right extensions.

The following file extensions are expected:

:file:`*.pem` or :file:`*.key`
    This holds the private key for the encryption. The key must be unlocked and in
    PKCS#1 format (see below).

:file:`*.crt`
    This is the certificate. Usually it is supplied by the certificate authority where you
    bought it. It can also be a self signed certificate, see below.

:file:`*.ca.crt`, :file:`cabundle.crt` or :file:`bundle.crt`
    This is the (optional) *CA bundle* that contains root and intermediate certificates for
    the certificate authority that issued the :file:`.crt` certificate.

    The certificate authority will supply these. All supplied certificates are
    concatenated, with the root certificate last.

    The concatenation is a literal command, like:

.. code-block:: bash

        cat intermediate.crt root.crt > cabundle.crt

Due to caching, it can take up to a minute before the new certificates are used.


Format of the private key
-------------------------

The Erlang SSL implementation accepts PKCS#1 and PKCS#8 format keys. OpenSSL generates (since 2010) PKCS#8
format keys.

A PKCS#1 key starts with:

.. code-block:: none

    -----BEGIN RSA PRIVATE KEY-----

A PKCS#8 key starts with:

.. code-block:: none

    -----BEGIN PRIVATE KEY-----

If there are problems then check if the ``.key`` or ``.pem`` file starts with one of the above strings.


Using SSL certificates
----------------------

If you order a SSL certificate, the signing authority will ask you which kind of web server you are using and a CSR file.
For the web server, select *other*. For the CSR, use the following command:

.. code-block:: bash

    openssl req -out certificate.csr -new -newkey rsa:2048 -nodes -keyout certificate.key

When OpenSSL asks for the *Common Name* then fill in the site’s hostname (e.g. *www.example.com*).

From the SSL certificate authority you will receive a signed ``.crt`` file and maybe a ``cabundle.crt`` file.

See the section *Certificate and key files* above for instructions how to use the ``.crt`` and ``.key`` files.


Generating a self signed certificate
------------------------------------

There is no need to make your own self signed certificate as Zotonic will generate one for every site.

Nevertheless, if you want to use your own self signed certificate, then run the following commmands:

.. code-block:: bash

    openssl req -x509 -nodes -days 3650 -subj '/CN=www.example.com' -newkey rsa:2048 \
         -keyout certificate.key -out certificate.crt

This generates a private key of 2048 bits and a certificate that is valid for 10 years.

.. seealso:: :ref:`mod_ssl_letsencrypt`, :ref:`ref-port-ssl-configuration`

