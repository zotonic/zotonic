
.. include:: meta-mod_ssl_ca.rst

The mod_ssl_ca module adds support for using SSL certificates bought from a Certificate Authority.

A free alternative to CA provided tickets is Let’s Encrypt, see :ref:`mod_ssl_letsencrypt`.

Certificate and key files
-------------------------

The files with the certificates and key are placed into the ``ssl`` directory inside the site
directory :file:`user/sites/sitename/ssl/ca/`.

Where *sitename* must be replaced with the name of your site.

The files all have the name of the site in them (*sitename* in the filenames below).
This is to prevent mixing them up with other sites:

:file:`user/sites/sitename/ssl/ca/sitename.pem`
    This holds the private key for the encryption. The key must be unlocked and in
    PKCS#1 format (see below).

:file:`user/sites/sitename/ssl/ca/sitename.crt`
    This is the certificate. Usually it is supplied by the certificate authority where you
    bought it. It can also be a self signed certificate, see below.

:file:`user/sites/sitename/ssl/ca/sitename.ca.crt`
    This is the (optional) *CA bundle* that contains root and intermediate certificates for
    the certificate authority that issued the :file:`sitename.crt` certificate.

    The certificate authority will supply these. All supplied certificates are
    concatenated, with the root certificate last.

    The concatenation is a literal command, like:

.. code-block:: bash

        cat intermediate.crt root.crt > sitename.ca.crt

Due to caching, it can take up to a minute before the new certificates are used.


Format of the private key
-------------------------

The Erlang SSL implementation uses PKCS#1 format keys. OpenSSL generates (since 2010) PKCS#8
format keys. The difference can be seen when inspecting the key file. A PKCS#1 key starts with:

.. code-block:: none

    -----BEGIN RSA PRIVATE KEY-----

Where a PKCS#8 key starts with:

.. code-block:: none

    -----BEGIN PRIVATE KEY-----

If mod_ssl sees that the key file is a PKCS#8 file then it will stop and log the following
error:

.. code-block:: none

    Need RSA private key file. Use: `openssl rsa -in ssl/ca/sitename.key -out ssl/ca/sitename.pem`

The given command is the command needed to convert the key to a PKCS#1 key. The
PKCS#8 key should be renamed to :file:`sitename.key` from :file:`sitename.pem`, before running the
above command.

Note that the resulting key file *must* be named :file:`sitename.pem` where *sitename* is the name of
the site the key is placed in.


Using SSL certificates
----------------------

If you order a SSL certificate, the signing authority will ask you which kind of web server you are using and a CSR file.
For the web server, select *other*. For the CSR, use the following command (replace ``sitename`` with
the name of your site):

.. code-block:: bash

    openssl req -out sitename.csr -new -newkey rsa:2048 -nodes -keyout sitename.key

When OpenSSL asks for the *Common Name* then fill in the site’s hostname (e.g. *www.example.com*).

The resulting ``.key`` file can be converted to a ``.pem`` file:

.. code-block:: bash

    openssl rsa -in sitename.key -out sitename.pem

From the SSL certificate authority you will receive a signed ``.crt`` file.

See the section *Certificate and key files* above for instructions how to use the ``.crt`` and ``.pem`` files.


Generating a self signed certificate
------------------------------------

If you want to use a self signed certificate, then run the following commmands (where ``sitename`` should
be replaced with the name of the site):

.. code-block:: bash

    openssl req -x509 -nodes -days 3650 -subj '/CN=www.example.com' -newkey rsa:2048 \
         -keyout sitename.key -out sitename.crt

This generates a private key of 2048 bits and a certificate that is valid for 10 years.

If the key is in PKCS#8 format (it starts with ``-----BEGIN PRIVATE KEY-----``), 
then run the following command as well:

.. code-block:: bash

    openssl rsa -in sitename.key -out sitename.pem

If the key is already in PKCS#1 format (it starts with ``-----BEGIN RSA PRIVATE KEY-----``) 
then just rename the file :file:`sitename.key` to :file:`sitename.pem`.


.. seealso:: :ref:`mod_ssl_letsencrypt`, :ref:`ref-port-ssl-configuration`

