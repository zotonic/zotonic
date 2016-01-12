Zotonic releases
================

Release date
------------

Zotonic follows a time-based release model. Every first Monday of the month – at
the call of the `Dutch test siren`_ – a new Zotonic version is released.

Schedule
--------

Preparation for each release lasts one month:

1. **Development phase**: new features are added and existing ones improved.
   Commits take place on the current ``.x`` development branch (for instance,
   ``release-0.13.x``).

2. **Stabilisation phase**: five working days before a release, we create a
   release branch (for instance, ``release-0.13.7``) from the development
   branch. During the stabilisation phase, no new features are added. Instead,
   the last bug fixes for the release are committed.

3. On the first Monday of each month, the release branch is tagged, merged back
   into the development branch and then discarded.

.. _Dutch test siren: http://www.invadingholland.com/guides-to-holland/emergency-alarm
