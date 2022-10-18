.. _rel-0.71.0:

Release 0.71.0
==============

Welcome to Zotonic 0.71.0, released on October 18, 2022.

This is a maintenance release.

Main changes are:

 * Force 2FA for accounts with certain user groups (this used to be set to 'nagging').
 * In the pager, round the number of estimated pages found. This prevents showing an arbitrary number that gives an exact impression. The number of pages is rounded to two significant digits.

Commits since 0.70.0
--------------------

Marc Worrell (3):

 * mod_base: add filter round_significant (0.x) (#3143)
 * mod_oauth2: change the per UG 2FA setting to '3' (force), instead of '2' (nagging) (#3154)

Rob van den Bogaard (1):

 * Fix identity set by type (#3138)
