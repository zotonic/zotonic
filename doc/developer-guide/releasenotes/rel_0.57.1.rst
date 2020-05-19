.. _rel-0.57.1:

Release 0.57.1
==============

Welcome to Zotonic 0.57.1, released on May 19, 2020.

Main changes are:

 * Fix a problem with https connections and OTP-22
 * Add a ``is_disabled`` flag to surveys

This updated mochiweb. Easiest upgrade method is to delete the *deps*
folder and let rebar refetch all dependencies.


Commits since 0.57.0
--------------------

Arjan Scherpenisse (1):

 * mod_survey - option to hide survey form (#2432)

Marc Worrell (1):

 * Upgrade mochiweb
