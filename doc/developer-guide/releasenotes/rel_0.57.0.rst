.. _rel-0.57.0:

Release 0.57.0
==============

Welcome to Zotonic 0.57.0, released on May 11, 2020.

Main changes are:

 * Make compatible with OTP-22
 * Fix a localtime problem for Theran/Iran timezone.
 * Fix for autostart of survey
 * Allow s3 filestore to retry on fetch errors
 * Add option to create a s3 bucket if it is not present

For the timezone and OTP-22 fix the following dependencies need to be updated:

 * mochiweb
 * qdate_localtime

Easiest method is to delete the *deps* folder and let rebar refetch
all dependencies.


Commits since 0.56.0
--------------------

Arjan Scherpenisse (1):

 * mod_survey bugfixes (#2429)

Maas-Maarten Zeeman (2):

 * Use zotonic's qdate_localtime to fix a problem when using Iranian tz settings (#2424)
 * Moved the qdate_localtime dep back to the original repo

Marc Worrell (8):

 * Fix a problem with cache invalidation after a resource merge.
 * Make compatible with OTP-22 (#2420)
 * mod_filestore: retry upload / download errors. Fix #2187 (#2272)
 * Allow to create s3 bucket when not present. (#2426)
 * mod_development: fix layout.
 * mod_filestore: set deleted date when deleting a file.
 * mod_filestore: cleanup and better handoff of file to cache.
 * mod_filestore: fix race condition. (#2427)
