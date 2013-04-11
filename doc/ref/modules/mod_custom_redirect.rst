
.. include:: meta-mod_custom_redirect.rst

Enables redirects from unknown hosts and paths to other locations.
The other location can be a known path or another web site.


Redirect unknown domains
------------------------

If the site dispatcher encounters an unknown host name then it notifies all modules with the ``#dispatch_host`` notification.

The Custom Redirect module observes this notification and checks against a configurable list of domains and redirects.
If a domain is matched then the site dispatcher will redirect the user agent to the new location.

The list of domains and their redirects can be configured in the admin: Modules -> Domains and redirects.

The domain must be like what is typed in the browser. Examples of domains are ``www.example.org`` and ``mypc.local:8000``.

Domain names are case insensitive, that is ``WWW.EXAMPLE.COM`` and ``WwW.Example.coM`` will both be matched with ``www.example.com``.
Contrary to the domain name, the path is case sensitive. That is ``/ABOUT`` and ``/About`` are two different paths and will need their own redirect rules!

The redirect location can be a complete URL (for example ``http://www.example.com/foo/bar.html``) or a path (for example ``/about``).


Redirect unknown paths
----------------------

After the site has been selected, the dispatcher matches the path to the dispatch rules.

When no dispatch rule matches, then the ``#dispatch`` notification is sent. The :ref:`mod_base` module observes that notification to check the path against the `page_path` properties of all resources. If :ref:`mod_base` didn’t find match then :ref:`mod_custom_redirect` will check all custom redirect with an empty domain and a matching path. The visitor will be redirected to the corresponding redirect location.


Permanent or temporary redirects
--------------------------------

A redirection can be permanent or temporary. A permanent redirect will be remembered by the visiting browser (and search engines), replacing any occurence of the redirected location. A temporary redirect will not be remembered and be retried on every visit.

.. seealso:: :ref:`manual-dispatch`, :ref:`mod_base`
