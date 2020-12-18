.. _rel-0.59.0:

Release 0.59.0
==============

Welcome to Zotonic 0.59.0, released on December 18, 2020.

This is a maintenance release and only includes fixes.

Main changes are:

 * Fix an issue where illegal URLs might be accepted for preview generation
 * Fix an issue wehere an export of a person did not include all expected properties

Commits since 0.58.0
--------------------

Marc Worrell (8):

 * Fix error message in m_rsc_update.
 * Fix a problem where the nowh image option could be added to the image url.
 * Fix a problem where generating a button-tag without text would omit the closing element.
 * Be more strict on acceptable formats for resized images.
 * mod_export: default disposition to 'attachment'
 * Log transport timeouts.
 * Small cleanup of media/file routines.
 * Remove wrong zh translation.

Rob van den Bogaard (1):

 * core: Fix m_rsc_export for person resources (#2548)
