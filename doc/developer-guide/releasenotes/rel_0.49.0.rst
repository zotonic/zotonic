.. _rel-0.49.0:

Release 0.49.0
==============

Welcome to Zotonic 0.49.0, released on May 6, 2019.

Main changes are:

  * **BC break:** Upgraded Bootstrap CSS and JavaScript in mod_admin to 3.4.1;
    mod_admin no longer ships with a separate Bootstrap instance but uses the one
    from mod_bootstrap. Any customizations you made to the Bootstrap CSS will
    therefore be visible in the admin, too.
  * Logging: the ``/admin/log`` changed and now more useful. Events logged include
    module activation, config changes, rate limiting and ACL changes.
  * ``mod_clamav`` now also rejects MS Office files with ``externalLinks`` directories.
    This can be turned off using the Zotonic config key ``clamav_reject_msoffice_external_links``
  * Uploaded CSV files are now given the mime type ``text/csv`` and properly sanitized
  * Password reset tokens are now valid for two days.
  * ``mod_linkedin`` now uses their v2 API

Commits since 0.48.1
--------------------

David de Boer (2):

 * mod_admin: Upgrade Bootstrap (#2041)
 * Add password reset token expiration (#2055)

Marc Worrell (5):

 * Linkedin v2 (#2048)
 * mod_linkedin: fix cherry-pick
 * Add config option to force new password on password reset (#2056)
 * Additional virus scanning for spreadsheet files (#2054)
 * Logging of system/acl events in mod_logging (#2057)

