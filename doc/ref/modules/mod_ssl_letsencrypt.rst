
.. include:: meta-mod_ssl_letsencrypt.rst

Request certificates from Let’s Encrypt.

`Let’s Encrypt <https://www.letsencrypt.com/>` provides free SSL certificates.

Zotonic can request these certificates automatically, easing deployment of https
secured web sites.


Hostname & port requirements
----------------------------

There are some criteria that must for each site requesting a certificate.

 1. Primary hostname(s) resolve using DNS
 2. Resolved DNS address is not a LAN address
 3. Site is reachable on the resolved address
 4. Listening for the hostname on that address

Zotonic *must* listen on http port 80 and ssl port 443 for connections.
If you use any other ports then requesting a certificate will fail.

See :ref:`ref-port-ssl-configuration` for more information about the configuring the correct
port numbers and optional proxy settings.


Requesting a certificate
------------------------

In the admin, go to System > Modules and ensure that ``mod_ssl_letsencrypt`` is enabled.

After mod_ssl_letsencrypt is enabled, go to System > SSL Certificates.

In the *Let’s Encrypt* panel you can request a certificate. Check the alternative names
you want to include in the certificates. (E.g. *example.com* and *www.example.com*).

The certificate request will run on the background and the status will be shown in the panel.

After a certificate was received, make sure that Let’s Encrypt is the first module on the
SSL Certificates list by disabling all modules above Lets’s Encrypt.

.. note: It can take a couple of minutes till the new certificate is used.

Now go to your site using https, you should be see your site protected by a Let’s Encrypt
certificate.


.. note:

  Let’s Encrypt certificates are valid for ninety days. Thirty days before expiration
  Zotonic will automatically try to fetch a new certificate. If an error happens then 
  the renewal will be retried daily during thirty days.


.. seealso:: :ref:`mod_ssl_ca`, :ref:`ref-port-ssl-configuration`
