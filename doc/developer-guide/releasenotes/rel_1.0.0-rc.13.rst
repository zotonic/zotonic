Release 1.0.0-rc.13
===================

Released on 2022-12-05.

This is a release candidate for Zotonic 1.0.

The code is stable and used in production. Some features will be added before the 1.0 release.

For the issues and changes to be added for the final 1.0.0 release, see:

https://github.com/zotonic/zotonic/milestone/3

The 1.0.0 release is expected in Q4 2022.


Updating
--------

After updating Zotonic, also update Cotonic with:

    make update-cotonic

This release needs the newest (master) version of Cotonic.

For the 1.0.0 we will start tagging the Cotonic versions.

Erlang/OTP Version
------------------

This release needs Erlang/OTP 23 or later.

Commits since 1.0.0-rc.12
-------------------------

Dorien Drees (1):

 * [Search] Refactor search view (#3196)

Maas-Maarten Zeeman (1):

 * mod_wires: Remove 2 char limit on typeselect search (#3194)

Marc Worrell (9):

 * doc: add 0.72.0 release notes
 * mod_authentication: fix a problem with pw reset for 2FA enabled accounts (#3190)
 * core: ensure that the media filename is always a binary (#3191)
 * Update zotonic_stdlib, template_compiler (#3193)
 * New pot file
 * mod_base: add filter translation (#3195)
 * Privacy billing (#3197)
 * mod_admin: Add optional editing of the page's timezone (#3201)
 * mod_base: Fix an issue where controller_page could enter in a redirect loop. (#3200)

Michiel Klønhammer (3):

 * Update email_password_reset.tpl
 * Merge branch 'master' of https://github.com/zotonic/zotonic
 * Merge branch 'master' of https://github.com/zotonic/zotonic

Mikael Karlsson (1):

 * mod_search: fix sorting for custom pivot (#3198) (#3199)
