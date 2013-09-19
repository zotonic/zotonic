HTTPS support
=============

HTTPS support for Zotonic versions 0.9 and up is handled by
:ref:`mod_ssl`. It allows each :term:`Zotonic site` to configure HTTPS
support, which runs on a different port for each site (due to the
limitations of HTTPS).

Configuring HTTPS for Zotonic 0.8.x and below
---------------------------------------------

Generate an RSA key and aquire the signed certificate. Also I used
`this simple doc
<https://knowledge.rapidssl.com/support/ssl-certificate-support/index?page=content&actp=CROSSLINK&id=so6411>`_
to get proper private key and CSR. Of course, you can also buy the
certificate at your favorite CA.

.. note:: The lines below are `really` just for Zotonic <= 0.8; they don't have any effect in 0.9 and up! Use :ref:`mod_ssl` instead.

Add to Zotonic’s priv/config file several lines::

  {listen_port_ssl,8443},
  {ssl,          true},
  {ssl_certfile,   "priv/ssl/mysite.pem"},
  {ssl_keyfile,    "priv/ssl/mysite.key"},
  {ssl_cacertfile, "priv/rapidssl.crt"}

If your certificate is protected by a password, you can also set it::

  {ssl_password,   "secret"},

Finaly, add `{ssl, true}` arguments to each dispatch rule that you want to protect::

  {home,   [],   resource_home,   [{ssl, true}, ...]},

This causes Zotonic to switch to the HTTPS version of the URL. You
need to adjust this for each dispatch rule you want to protect.
  
Finally, restart Zotonic. You should be set!


