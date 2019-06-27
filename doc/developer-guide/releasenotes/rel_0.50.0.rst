.. _rel-0.50.0:

Release 0.50.0
==============

Welcome to Zotonic 0.50.0, released on June 27, 2019.

Main changes are:

  * Compatible with OTP 21, minimal OTP version is now 18
  * View a log of Javascript UI errors in ``/admin/log/ui`` (disable with ``mod_logging.ui_log_disabled``)
  * Log more events like identity changes and applied rate limit throttling
  * Only the admin can now chnage 2FA settings of the admin
  * In the admin connect dialog a new filter on category is added
  * Fix a problem with password resets on expired passwords
  * Fix a problem with password resets when the new password matches the old one
  * Fix a problem with wrong orientation of portrait video on Linux
  * Accept resource updates with ``dmy`` formatted date values

The ``exometer`` application was wrongly initialized since it was split into ``exometere_core``
and other applications. The default erlang configuraton had an entry for ``exometer``, you
should rename this in your ``erlang.config`` file to ``exometer_core``.


Commits since 0.49.2
--------------------

David de Boer (2):

 * build: Test 0.x against Erlang/OTP 21 (#1936)
 * mod_seo: Don't store noindex value in db when read from site config (#2081)

Dorien (1):

  * mod_admin: Convert predicate to binary to fix matching (#2091)

Maas-Maarten Zeeman (1):

 * Exometer core config update (#2095)

Marc Worrell (17):

 * core: fix a problem where a filename with a space failed identify. (#2080)
 * Log levels and extra checks for identity changes / ratelimit (#2078)
 * Allow all categories if no predicate is defined. Fixes #2083 (#2085)
 * mod_admin: make upload element id dynamic to prevent id clash.
 * Log client ui errors (#2086)
 * Fix race condition in rsc_gone (#2087)
 * Only allow admin to change admin 2fa settings (#2077)
 * In connect/create dialog, add extra filter on category.
 * Fix link in body, hide 'link' when editing hierarchies.
 * WIP: Ensure 'rsc' table ref on search sort terms (#2035)
 * mod_logging: add config mod_logging.ui_log_disabled
 * Fix a problem with searching on 'id:..' in the create/link dialog. Issue #2089 (#2092)
 * admin: show 'unique name' field in quick edit for all meta resources. (#2097)
 * mod_video: cleanup and fix rotation on linux (#2099)
 * Support 'dmy' input format for dates. (#2098)
 * mod_ratelimit: add 'm_ratelimit:delete_event/3' to remove a ratelimit
 * mod_admin: do not toggle 'depiction' if image is embedded in tinymce (#2103)
