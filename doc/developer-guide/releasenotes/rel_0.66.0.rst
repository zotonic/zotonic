.. _rel-0.66.0:

Release 0.66.0
==============

Welcome to Zotonic 0.66.0, released on May 24, 2022.

This is a maintenance release.

Main changes are:

 * Ensure in sent emails that the 'To' field is properly formatted
 * Do not crash in m_edge:objects/3 and subjects/3 on non existing resources
 * The image template tag now supports the 'decoding' attribute

Commits since 0.65.0
--------------------

Colin de Roos (1):

 * Fix syntax error in zotonic.config.in (#2941)

Marc Worrell (4):

 * core: on image tag, support the 'decoding' attribute
 * core: log sass errors to the console log as info messages
 * core: in m_edge, accept non existing resources for objects/3 and subjects/3
 * core: ensure To email is properly formatted. (#2968)
