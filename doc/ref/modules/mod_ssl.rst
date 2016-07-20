
.. include:: meta-mod_ssl.rst

The mod_ssl module adds HTTPS support. After enabling mod_ssl the logon window
and other secure pages will be served using HTTPS.

SSL support can be switched on for each site separately. Virtual hosting of sites via
HTTPS is possible. In order to get working SSL connections you need to enable a module
providing SSL certificates. Module :ref:`mod_ssl_self_signed` is an example of such a 
module. The role of this module is secure dispatching.


Configuration
-------------

There are four configurarion options. They can be set in the admin. All four are optional
and will be either set or replaced with a default when not set.

``mod_ssl.is_secure``
    When this configuration is set to something else then ``0`` or ``false`` then mod_ssl will
    ensure that sessions started while using HTTPS are secured and only valid on HTTPS.

    The *autologon* cookie will also be HTTPS only (if set from a HTTPS connection).

    Besides that mod_ssl will ensure that, after using HTTPS, all following pages will be
    served using HTTPS. Unless specifically specified otherwise in the dispatch rules.

    This is done by adding the ``{ssl, keep}`` option to all dispatch rules that do not have
    an ``ssl`` option already.

``mod_ssl.is_ssl``
    Force all dispatch rules to use ``{ssl, true}``, unless specified otherwise.
    Use this in combination with ``mod_ssl.is_secure`` to ensure serving a site over HTTPS.

``mod_ssl.is_permanent``
  When set to ``true`` it makes http to HTTPS protocol redirects for dispatch rules which
  use ``{ssl, true}`` to be permanent redirects. The default setting is ``false`` which will
  make redirects temporary redirects. 


Erlang SSL Configuration
------------------------

The erlang ssl application is configured in the :file:`priv/erlang.config`. If this file is 
missing then it can be copied from :file:`priv/erlang.config.in`.  It contains a couple of 
important settings which we recommend you to change. The reason for this is that the default 
settings Erlang uses are unsuitable for web servers. The most important settings are listed 
below.

``session_lifetime``
  Sets the maximum lifetime of session data in seconds. 

``session_cache_server_max``
  Sets the maximum number of client sessions cached by the server. 

For more information on configuration options, please see `Erlang SSL App`_.


Serving a page via SSL
----------------------

The :ref:`dispatch rule argument <guide-dispatch>` ``ssl`` defines if a page will be
served over HTTPS or HTTP.

There are three variations:

``{ssl, any}``
    Keep the same protocol as before, don‘t switch beteen HTTP and HTTPS.
    This used for lib and image files.

``{ssl, true}``
    Force a switch to HTTPS. When accessing the page using http then the page will
    be reloaded using HTTPS.
    This is useful for logon, logoff and other authentication or secure pages.

``{ssl, false}``
    Force a switch to HTTP. When accessing the page using HTTPS then the page will
    be reloaded using HTTP.
    This is useful for pages with embedded video or other non HTTPS content.

When the ``ssl`` option is not specified then it defaults to ``{ssl, false}``. Unless
the ``mod_ssl.is_secure`` option is set, then default is ``{ssl, any}``.

Example of a page delivered using HTTPS::

    {logon,  ["logon"], controller_logon, [{ssl, true}]}

And of a dispatch rule that should keep the protocol, in this case the lib controller
used for serving css, javascript and other static lib files::

    {lib, ["lib",'*'], controller_lib, [{ssl, any}]}


Notifications
-------------

When you want to implement your own certificate handling you have to implement a 
notification handler which returns the certificates to the underlying 
HTTPS server. This can be needed when you have a site with different aliases, or when 
you can want to implement automated certificate handling for a specific certificate 
authority.

``ssl_options{server_name=ServerName}``
  Sent back the certificate, key or other ssl options. ``ServerName`` is the
  name of the server found in the SSL handshake. Expects a proplist with Erlang 
  ``ssl:ssloptions()``. This proplist will override the default ssl options for this 
  connection. For more information about the possible properties see `Erlang SSL`_. 
  When ``undefined`` is returned the SSL handshake will fail and the connection wil 
  be dropped.
  
  
.. _Erlang SSL: http://erlang.org/doc/man/ssl.html
.. _Erlang SSL App: http://erlang.org/doc/man/ssl_app.html

