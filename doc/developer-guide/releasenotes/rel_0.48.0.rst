.. _rel-0.48.0:

Release 0.48.0
==============

Welcome to Zotonic 0.48.0, released on 9 April, 2019.

Main changes are:

 * ``mod_geoip`` to lookup Geo information for an IP address
 * New connect dialog in the admin
 * Option to let users agree to changed Terms & Conditions after logon
 * Changes to ``mod_clamav`` virus scanning to allow uploads of files larger than the
   maximum file size for clamd.

Commits since 0.47.0
--------------------

David de Boer (1):

 * Split HTML head templates to make overrides possible (#1746) (#2017)

Dorien (1):

 * admin: Add block around modal footer (#2018)

Maas-Maarten Zeeman (3):

 * core: Template controller accidentally used the wrong context variable
 * mod_bootstrap: Upgrade to 3.4.1 and add a upgrade description. Fixes #2020 (#2021)
 * Locked new diffy and z_stdlib. Fixes diffing bug and adds options for encoding ubf

Marc Worrell (20):

 * admin: Combine new-page, upload, and connect dialog (#2019)
 * mod_clamav: fix default max size.
 * Upgrade z_stdlib
 * Lock new z_stdlib
 * Fix a problem where the new find/new connect dialog failed with other ACL modules and with linking in texts.
 * mod_geoip: new module to map IP addresses to country names. (#2025)
 * mod_geoip: add ip2info filter to extract more information about an IP address
 * Rename filter ip2info to ip2geo
 * mod_admin_identity: let admins not switch to the 'admin' account.
 * mod_admin: fix error message display on errornous media upload.
 * mod_content_groups: give feedback why a content group could not be deleted. (#2023)
 * Tos agree on create (#2022)
 * mod_search: fix a problem with specifying 'qargs' in a stored query. Fixes #2026
 * Suppress backup files for dispatch rules.
 * mod_clamav: fix spec
 * mod_acl_user_groups: reconnect all websocket connections of an user if their permissions change. Fixes #2028 (#2029)
 * Redesign of connect/new tab. (#2034)
 * Fix a problem with the preflight_check  of a query with 'qargs' term.
 * clamav: set medium flags 'is_av_scanned' and 'is_av_sizelimit' (#2031)
 * New z_stdlib
 * Fix a problem with the menu editor and the new connect/find dialog. Fixes #2038
