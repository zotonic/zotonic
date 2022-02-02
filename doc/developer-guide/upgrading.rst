.. _upgrade-notes:

Upgrade notes
=============

These notes list the most important changes between Zotonic
versions. Please read these notes carefully when upgrading to a new
major Zotonic version.

Upgrading from 0.x to 1.0
-------------------------

Before upgrading to 1.0, be sure to check the upgrade notes for the latest 0.x version, especially if
you are upgrading from before 0.12. See `the upgrade notes on 0.x <https://github.com/zotonic/zotonic/blob/0.x/doc/developer-guide/upgrading.rst>`_ for more information.


Erlang/OTP
^^^^^^^^^^

Support for Erlang versions before 22 was dropped.

Zotonic modules are now separately published as packages to `Hex.pm`_, which
allows you to build your own Zotonic distribution and to have each of your
sites depend on the Zotonic modules (and other Erlang packages) it needs.

This was done by restructuring Zotonic into an `umbrella application`_. The
``src/`` directory was moved to new ``zotonic_core`` app.

Before::

    -include_lib("zotonic.hrl").

After::

    -include_lib("zotonic_core/include/zotonic.hrl").

The HTTP and SMTP listeners were moved to a new ``zotonic_listen_http`` and
``zotonic_listen_smtp`` app respectively.

A ``zotonic_launcher`` app was introduced for starting Zotonic.

All built-in modules, the testsandbox and the status sites are now to be found
in the ``apps/`` directory.


Docker
^^^^^^

Only the `zotonic/zotonic-dev <https://hub.docker.com/r/zotonic/zotonic-dev/>`_ image
is now available and automatically updated.

We have decided to drop the other Docker images as in practice everybody was creating
their own production images anyway.

All files used by the Docker container are now placed in the :file:`docker-data` directory.

See for more information :ref:`guide-docker`.


Sites and modules are now OTP apps
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Both sites and modules now follow the standard `OTP directory structure`_,
which means all Erlang files should reside in :file:`src/` and all other files
(templates, dispatch rules etc.) in :file:`priv`/.

Before::

    yoursite/
        models/
            m_some_model.erl
        templates/
            some_template.tpl
        yoursite.erl
        config
        ...


After::

    yoursite/
        priv/
            zotonic_site.config
            templates/some_template.tpl
            ...
        src/
            models/m_some_model.erl
            yoursite.erl
            yoursite.app.src
            ...
        rebar.config

The ``user_sites_dir`` and ``user_modules_dir`` configurations have been removed.
The default location for sites and modules is now the ``apps_user`` directory.
With the ``ZOTONIC_APPS`` environment variable you can define an additional source directory
outside the Zotonic umbrella ``apps`` directory.

To upgrade, move your ``user/modules`` and ``user/sites`` applications to the ``apps_user``
directory.


Resources
^^^^^^^^^

All resources are now *maps* with *binary* keys. Use of the previous 0.x
proplists is deprecated, the fetch routines will always return maps, the
update routines will convert property lists to maps before updating.

The ``name_to_id_check/2`` functions were removed from ``m_category``,
``m_predicate`` and ``m_rsc``.

Before::

    Id = m_rsc:name_to_id_check(Value, Context).

After::

    {ok, Id} = m_rsc:name_to_id(Value, Context).

Inserting or deleting an edge no longer modifies the last modified and
modifier properties of the edge’s subject resource.

There are extra access controls on rsc properties. The ``privacy`` property
controls what is visible for whom.

The function ``m_rsc:get_visible/2`` has been removed. The function ``m_rsc:get/2``
now checks on visibility of properties. To fetch all properties, either  use ``m_rsc:get_raw/2``
or call ``m_rsc:get/2`` as a administrator level user.

Media
^^^^^

The medium record is now a *map* with *binary* keys. Use of the previous 0.x
proplists is deprecated, the fetch routines will always return maps, the
update routines will convert property lists to maps before updating.


ACL
^^^

mod_acl_adminonly was replaced by :ref:`mod_acl_user_groups`. To create users
that have access to the admin, add them to the ‘Managers’ user group.

The ``visible_for`` property semantics and the the ``acl_can_see``
notification were removed. You can get similar functionality by adding users
to user and collaboration groups. These are provided by mod_acl_user_groups.
The ``visible_for`` ``rsc`` table property has been kept for BC. So if you’re
using mod_acl_adminonly, mod_acl_simple_roles or a custom ACL module you can
still rely on the property.

The ``acl_rsc_update_check`` notification was removed.

Authentication
^^^^^^^^^^^^^^

All auth notifications values were converted to records.

Before::

    observe_auth_logon(auth_logon, Context, _Context) ->

After::

    observe_auth_logon(#auth_logon{}, Context, _Context) ->


Configuration
^^^^^^^^^^^^^

Port configuration :ref:`environment variables <guide-deployment-env>` were
changed.

Before:

  .. code-block:: bash

    ZOTONIC_PORT=80 ZOTONIC_SSL_PORT=443 bin/zotonic start

After:

  .. code-block:: bash

    ZOTONIC_LISTEN_PORT=80 ZOTONIC_SSL_LISTEN_PORT=443 bin/zotonic start

Black/white-lists are now called block/allow-lists.

 - ``proxy_whitelist`` is now ``proxy_allowlist``
 - ``smtp_dnsbl`` is now ``smtp_dns_blocklist``
 - ``smtp_dnswl`` is now ``smtp_dns_allowlist``
 - ``ip_whitelist`` is now ``ip_allowlist``
 - ``ip_whitelist_system_management`` is now ``ip_allowlist_system_management``

If an IP is on DNS allowlist then ``z_email_dnsbl:status/2`` returns now ``{ok, allowed}``.


Errors
^^^^^^

``m_edge``, ``m_identity``, ``m_rsc``, ``m_rsc_import`` and ``m_rsc_update``
no longer throw exceptions. Instead, they return an ``{error, atom()}`` tuple
on failure.

Before::

    m_edge:insert(Id, this_predicate_does_not_exist, UserId, Context).
    %% crashes with an exception

After::

    m_edge:insert(Id, this_predicate_does_not_exist, UserId, Context).
    %% fails silently, so to make it crash:

    {ok, _EdgeId} = m_edge:insert(Id, this_predicate_does_not_exist, UserId, Context).

    %% alternatively:
    case m_edge:insert(Id, this_predicate_does_not_exist, UserId, Context) of
        {ok, _EdgeId} ->
            "Everything fine!";
        {error, Reason} ->
            "Something went wrong!"
    end.


Logging
^^^^^^^

.. seealso:: :ref:`cookbook-logstash`

The ``lager`` logger has been removed and replaced with the standard `erlang logger application`_.

For this to work:

 * Add the logger configuration to the ``erlang.config`` file. See :ref:`dev-logging` for an example.
 * Remove the ``lager`` definition from the erlang.config file.


Export
^^^^^^

Modules mod_atom and mod_atom_feed were removed. You can export data in a
variety of formats using :ref:`mod_export`.

JSON
^^^^

Mochijson structures replaced with Erlang maps.

All JSON encoding/decoding now relies on JSX and goes through
``z_json:encode/1`` and ``z_json:decode/1``.

``{trans, _}`` tuples should now be unpacked by the client, before calling
``z_json:encode/1`` (previously ``z_json:to_mochijson/2``).

Removed or deprecated functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Deprecated functions have been removed from ``z_utils``. Use the ``z_url`` and
``z_json`` modules instead.

Deprecated function ``z_utils:name_for_host/2`` has been removed; use
``z_utils:name_for_site/2`` instead.

The ``{% stream %}`` tag was removed, use MQTT websocket instead

Removed older TinyMCE versions 3.5.0 and 4.2.4.

``z_utils:combine/2`` is removed, use ``lists:join/2`` instead.

``z_utils:combine_defined/2`` is renamed to ``z_utils:join_defined/2``.

Module schema and data initialization
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The `#datamodel.data` field has been removed.
The notifier `#manage_data` has also been removed.

Now the call to (the optional) `manage_schema/2` will be followed by a call
to `manage_data/2`. Note that `manage_data` will be called if and only if
you have a `manage_schema/2` function exported (and the `-mod_schema(..)`
version changes or the module is installed).

The `manage_schema/2` function is called inside a transaction. The
`manage_data/2` function is called after that transaction and also after
all (optional) `#datamodel` changes are applied.


Templates
^^^^^^^^^

The ``use_absolute_url`` argument of the ``url``, ``image`` and ``lib`` tags
was renamed to ``absolute_url``.

Templates are now stored in :file:`yoursite/priv/templates/` instead of
:file:`yoursite/templates/`.

The ``maxage`` caching argument was renamed to ``max_age``.

The models have now extra ACL checks.

The ``m.config``, ``m.site`` and ``m.sysconfig`` models are only accessible
as administrator. Use the models *owning* the various settings to access the
configurations.

Exception is that the hostname and site-title information is publicly accessible
using ``m.site``.

Examples:

   * ``m.config.site.title.value`` is now ``m.site.title``
   * ``m.config.mod_editor_tinymce.version.value`` is now ``m.editor_tinymce.version``

Check the various models of the modules for the new lookups.

The ``catinclude`` for a resource with an unique name will not look for (assuming
the unique name is ``my_unique_name`` and the template is ``page.tpl``):
``page.name.my_unique_name.tpl`` and **not** anymore for ``page.my_unique_name.tpl``.
Rename your templates accordingly.

The category property ``feature_show_address`` property is now called ``is_feature_show_address``. All
feature properties should be called ``is_feature_...`` to obtain a proper boolean value
after the category edit form is saved.

Port, proxies and SSL certificates
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

SSL/https support has been completely refactored.

 * SSL self signed certificates have been moved into the core
 * New modules :ref:`mod_ssl_ca` and :ref:`mod_ssl_letsencrypt`
 * Deleted module ``mod_ssl``
 * Port configuration has been changed, see :ref:`ref-port-ssl-configuration`
 * If you have a ``priv/ssl`` directory in your site, rename it to ``priv/security`` 

For an overview of https support, see :ref:`https-support`


Erlang code, Controllers, Event handlers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you made a site using custom controllers or request handling then you need to adapt your Erlang code.
Zotonic is now using Cowboy under the hood for the http handling, previously this was MochiWeb.

The following changes are made:

 * Binaries for all request variables and arguments.
 * Events use binaries for strings in templates.
 * Cookies are binaries.
 * Request headers are binaries.
 * Controllers initialization callbacks are removed.
 * Controller callbacks have a single *Context* argument.
 * Custom websocket handlers are removed, implement your own using Cowboy.
 * The include file ``include/controller_webmachine_helper.hrl`` is removed (and not needed anymore).

Binaries for request variables
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you request an argument with ``z_context:get_q/2`` and related functions then you might need to adapt some code. Requesting a query argument using an *atom* or *binary* will return a *binary*. Requesting with a *string* returns a string, this is for backwards compatibility. The function ``get_q_all`` will return all arguments as binaries.

In short:

  * ``z_context:get_q(<<"arg">>, Context)`` returns ``<<"value">>``
  * ``z_context:get_q(arg, Context)`` returns ``<<"value">>``
  * ``z_context:get_q("arg", Context)`` returns ``"value"``
  * ``z_context:get_q_all(Context)`` returns ``[ {<<"arg">>,<<"value">>}, ...]``

The binary name is the preferred way to request arguments.


Events like submit, postback and postback_notify
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Strings in the ``#submit{}``, ``#postback{}``  and ``#postback_notify{}`` events are now binaries. This is especially the case for the message, trigger, target, and form fields.

For example, replace ``#submit{message="hello"}`` with ``#submit{message = <<"hello">>}``.
Watch the space between ``=`` and the ``<<"...">>``, without the space you will get a syntax error.


Cookies
^^^^^^^

Use binaries for fetching and setting cookie names and values, don't use strings.


Request and response headers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

All request and response headers now use binary names and values, do not use strings.

The request and response header names are normalized to lowercase names, so always use ``<<"x-my-header">>`` and *never* ``<<"X-My-Header">>``.

The header values are passed as-is, and they are always binaries.


Controllers
^^^^^^^^^^^

The controllers are simplified and will need some adaptations.

The following callbacks are removed:

 * ``init``
 * ``ping``

All other callbacks have now a single *Context* argument, the *ReqData* argument has been removed.
There is no need anymore for the ``?WM_REQ`` and ``?WM_REPLY`` macros, and they have been removed.

Other controller changes changes are:

 * Content types are now binaries in `content_types_accepted` and `content_types_provided`
 * Character sets are now binaries in `charsets_provided`
 * Methods are now binaries in `allowed_methods` and `known_methods`
 * Encodings are now binaries in `content_encodings_provided`
 * The return value of `generate_etag` must be a binary

Search
^^^^^^

Search argument ``authoritative`` was renamed to ``is_authoritative``.

The ``custompivot`` has been removed. Pivot fields can now directly be addressed with ``pivot.mypivotname.column``.
Pivot tables are now joined automatically, removing the need for the ``custompivot`` search argument.

Notifications
^^^^^^^^^^^^^

The ``admin_menu`` notifications is now a tuple: ``#admin_menu{}``. Update the ``observe_admin_menu`` functions in sites and modules.


Modules
^^^^^^^

Moved ``mod_base_site`` to https://github.com/zotonic/zotonic_mod_base_site


Menus
^^^^^

The storage format of the `menu` property is changed.  Previously it was stored as a list of tuples::

    {1234, [ {5678, [ ... ]}, ...}

This doesn't allow for conversion to JSON, so the structure has been changed to use records::

    #rsc_tree{ id = ...,  tree = [ ... ]}

This allows for serialization to JSON using jsxrecord.



.. _OTP directory structure: http://erlang.org/doc/design_principles/applications.html#id82228
.. _umbrella application: https://www.rebar3.org/v3/docs/from-rebar-2x-to-rebar3#section-required-directory-structure
.. _Hex.pm: https://hex.pm
.. _erlang logger application: https://www.erlang.org/doc/apps/kernel/logger_chapter.html
