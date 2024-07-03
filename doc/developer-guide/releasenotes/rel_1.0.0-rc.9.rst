Release 1.0.0-rc.9
==================

Released on 2022-10-12.

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

Commits since 1.0.0-rc.8
------------------------

Marc Worrell (13):

 * docs: release notes 0.70.0
 * mod_authentication: add erljwt (#3134)
 * mod_oauth2: fixes for fetching new certificates. (#3135)
 * mod_filestore: add ftps support (#3137)
 * docs: add 0.70.1 release notes
 * mod_zotonic_site_management: add blocks templates to blog skel (#3109)
 * mod_backup: less backups, upload to filestore (#3141)
 * mod_backup: fix for periodic upload
 * Upgrade s3filez
 * mod_base: add filter round_significant (#3142)
 * mod_admin: round estimated number of found rows in search results
 * core: in logger show unicode binaries as strings (#3144)
 * Upgrade mqtt_sessions to 2.1.0

Michiel Klønhammer (3):

 * New Crowdin updates (#3136)
 * New Crowdin updates (#3139)
 * New Crowdin updates (#3145)
