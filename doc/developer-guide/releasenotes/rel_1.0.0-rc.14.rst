Release 1.0.0-rc.14
===================

Released on 2022-12-23.

This is a release candidate for Zotonic 1.0.

The code is stable and used in production. Some features will be added before the 1.0 release.

For the issues and changes to be added for the final 1.0.0 release, see:

https://github.com/zotonic/zotonic/milestone/3

The 1.0.0 release is expected in January, 2023.


Updating
--------

After updating Zotonic, also update Cotonic with:

    make update-cotonic

This release needs the newest (master) version of Cotonic.

For the 1.0.0 we will start tagging the Cotonic versions.

Erlang/OTP Version
------------------

This release needs Erlang/OTP 23 or later.

Commits since 1.0.0-rc.13
-------------------------

MM Zeeman (1):

 * core: Support wepb in image tag (#3226)

Marc Worrell (16):

 * mod_admin: Better warnings and texts for time zone info. (#3207)
 * New pot file
 * docs: add 0.73 release notes.
 * core: fix paging in m_search with payload (#3212)
 * mod_search: add is_unfindable and custom query terms (#3213)
 * New pot file
 * Also update query to add '= true' for better index usage (#3216)
 * mod_search_query: fix a problem with filtering facets (#3217)
 * mod_admin: better user create email, fix race condition (#3218)
 * New pot
 * mod_search: fix a problem with joining search_facet table (#3220)
 * mod_admin: add admin notes; mailinglist opt out (#3222)
 * New pot
 * core: allow send to recipient-id email address if recipient is visible. (#3227)
 * mod_acl_user_groups: add notification to update an user's user groups on lookup (#3225)
 * mod_mailinglist: allow multiple mailinglists subscribe forms on one page (#3223)

Michiel Klønhammer (4):

 * New Crowdin updates (#3208)
 * New Crowdin updates (#3214)
 * New Crowdin updates (#3219)
 * New Crowdin updates (#3224)
