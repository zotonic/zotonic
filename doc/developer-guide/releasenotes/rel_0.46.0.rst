.. _rel-0.46.0:

Release 0.46.0
==============

Welcome to Zotonic 0.46.0, released on 15 February, 2019.

Main changes are:

 * Added ``mod_auth2fa`` for two factor authentication support
 * Added ``mod_clamav`` for virus scanning of media items
 * Newly entered passwords can now be checked against a regex, configured in ``mod_admin_identity.password_regex``
 * New versions of tinyMCE, jQuery, and Bootstrap.

Commits since 0.45.0
--------------------

David de Boer (6):

 * core: Upgrade jquery-ui to 1.12.1, which fixed CVE-2016-7103 (#1992)
 * core: Embed YouTube over HTTPS (#1983)
 * mod_admin_identity: Check password regex for new users (#1997)
 * core: Add cors_headers notification (#1993)
 * mod_bootstrap: Upgrade to 3.4.0 (#1991)
 * mod_authentication: Fix config check (#2003)

Marc Worrell (9):

 * Add two-factor authentication (TOTP) (#1987)
 * mod_authentication: Autocomplete off on password field
 * zotonic_status: add optional ip whitelist for accessing the zotonic status site. (#1990)
 * mod_clamav: Scan for viruses in uploaded files with ClamAV (#1994)
 * mod_editor_tinymce: upgrade to 4.9.3 (#1999)
 * mod_authentication: add config to enable reminder email to unregistered addresses. (#1998)
 * mod_twitter: Add configurable poll delay. Issue #1985 (#2002)
 * Lock new webzmachine
 * mod_clamav: fix error message for tcp connect errors.
 * mod_signup: check password against configured regex.

Rob van den Bogaard (1):

 * [mod_admin] Don't let flush button in admin interface flush all sites (#2000)
