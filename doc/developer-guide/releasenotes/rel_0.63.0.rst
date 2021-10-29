.. _rel-0.63.0:

Release 0.63.0
==============

Welcome to Zotonic 0.63.0, released on Oct 29, 2021.

This is a maintenance release.

Main changes are:

 * Fix for a problem where deactivating languages on an edit page could leave the deactivated language selected for editing
 * Fix for a problem where the column of the UI log was too small for some IPv6 addresses
 * Dutch translations added to mod_google and mod_facebook


Commits since 0.62.0
--------------------

Dorien (1):

 * Update link to facebook dev apps (#2761)

Maas-Maarten Zeeman (1):

 * Fixed typo (#2739)

Marc Worrell (5):

 * Fix a problem with activating languages on the edit page where an inactivated language might stay visible.
 * Show 2FA key below QR code.
 * Fix a problem where the ip address field of log_ui was too small for some IPv6 addresses.
 * typo in nl translation
 * Add FB translations
