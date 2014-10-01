Release 0.10.2
==============

Released on 2014-10-01 20:16 by arjan.


Arjan Scherpenisse (13):

* mod_menu: Add menuexport JSON service
* mod_survey: Return the (possibly generated) submission id, after insert
* mod_admin: Put the unicode quotes of the link message in binaries
* core: Flush z_datamodel after managing new categories
* mod_twitter: Fix twitter OAuth logon
* mod_base: Export controller_logon:set_rememberme_cookie/2
* doc: clarify using categories defined by other modules in manage_schema
* core: Always normalize rsc blocks
* mod_survey: Align ACL check of printable results page
* core: Fix name-to-id cache invalidation
* mod_admin: Add option to create outgoing edges in new page dialog.
* mod_survey: Fix dispatch rule ACL access
* mod_oauth: No ACL check on authorize URL; just check if logged in

Arthur Clemens (19):

* minor translation and typo fixes
* docs: small improvements to link and unlink action
* doc: mention named wire actions in wire doc
* Remove redundant is_protected check on resource
* doc: update menu documentation to reflect actual output of the module
* mod_menu: add param class to menu
* doc: add reference to to_integer
* core: increase maximum image size for previews
* doc: describe attributes removebg and upscale
* doc: document md5 filter
* doc: document filter default
* doc: fix module install command
* translation: typo
* Fix various NL translation inconsistencies
* Fix NL translation
* Only use pre block if error_dump is available
* Add clause for 404
* mod_acl_simple_roles: add module names to titles
* mod_oauth: change required access to mod_oauth

David de Boer (3):

* Fix edges not being added in manage_schema
* Allow to set is_protected from manage_schema
* mod_admin: Fix typo

Marc Worrell (12):

* mod_video_embed: correctly fetch preview images for youtube videos with a '#' in the url.
* mod_video: fixes for handling binaries returned from the z_string and other function (used to be lists)
* Merge branch 'release-0.10.x' of github.com:zotonic/zotonic into release-0.10.x
* core: minor fix in mime-lookup for files not recognized by 'identify'
* core: allow binary results from validations. For now map them to lists, later with the binary webmachine replace this with binary only.
* core: fix problem where rsc_gone insertion would crash if there is already a 'gone' entry for the rsc. This is in the category 'should not happen', but also happened in production.
* core: add infinite timeout to dispatcher calls, prevent crashes on busy or swapping machines.
* core: add DB_PROPS macro to ease transition from 0.10 to 0.11
* Merge pull request #799 from driebit/is-protected-schema
* mod_survey: better survey results display.
* core: fixes for tinymce initialization and language tab sync in the admin
* mod_admin_identity: let's stop sending the password in clear text.

