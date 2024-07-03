Release 1.0.0-rc.12
===================

Released on 2022-11-17.

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

Commits since 1.0.0-rc.11
-------------------------

Marc Worrell (6):

 * Upgrade zotonic_stdlib to 1.11.2
 * mod_video_embed: fix a problem where embed could crash on illegal vimeo links (#3180)
 * core: use SQL 'any' instead of 'unnest' (#3182)
 * mod_base: fix postback test (#3183)
 * mod_survey: set answer_user_id to user if not editing existing answer (#3185)
 * Fix typo (#3186)
 