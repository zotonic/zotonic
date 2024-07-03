Release 1.0.0-rc.10
===================

Released on 2022-10-31.

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

Commits since 1.0.0-rc.9
------------------------

Marc Worrell (8):

 * workflows: add manual trigger for publish workflow (#3149)
 * Use ex_doc for the hex docs (#3150)
 * mod_admin: add column published-on to overview list table (#3153)
 * docs: add release notes 0.71.0
 * mod_translation: change x-default url to use 'en' (#3159)
 * Schema medium import (#3158)
 * mod_ssl_letsencrypt: ensure that connections to LE are closed (#3160)
 * zotonic_mod_base: fix for controller_static_pages with {files, ...} root

Michiel Klønhammer (5):

 * New translations zotonic.pot (Russian) (#3147)
 * New Crowdin updates (#3148)
 * New Crowdin updates (#3151)
 * New Crowdin updates (#3152)
 * New Crowdin updates (#3155)

Radek Szymczyszyn (1):

 * Fix -include{,_lib} attributes to unbreak doc chunk generation (#3146)
