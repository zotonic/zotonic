.. _rel-0.49.2:

Release 0.49.2
==============

Welcome to Zotonic 0.49.2, released on May 15, 2019.

Main changes are:

  * Fix a problem with password resets on expired passwords
  * Fix a problem with password resets when the new password matches the old one

Commits since 0.49.1
--------------------

Marc Worrell (3):

 * core: fix a problem with site.password_force_different where the user could be redirected if the passwords matched.
 * mod_import_csv: allow device pid to be passed to parse_csv:scan_lines/2
 * mod_authentication: fix a problem with reseting expired passwords. (#2060)

