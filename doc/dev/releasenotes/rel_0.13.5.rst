.. _rel-0.13.5:

Release 0.13.5
==============

Welcome Zotonic 0.13.5, released on October 27, 2015.

This is a maintenance release of Zotonic 0.13

Main changes are:

 * Updates to the bundled tinymce and code view plugins
 * Fulltext search fixes, search terms could be stemmed multiple times
 * Many fixes to the admin css and html
 * Fixes an ACL user groups problem where the sudo permissions didn't reflect the admin
 * Fixes a problem with the template compiler where a template could be compiled multiple times
 * Translation option to force the default language of a site
 * Fixes for the comet and postback controllers where push data was not sent with postback requests
 * New admin filter 'temporary_rsc'
 * Fix for a problem in mod_video where a video could be rendered with a color space that is incompatible with QuickTime


Commits since 0.13.4
--------------------

There were 101 commits since release 0.13.4.

Big thanks to all people contributing to Zotonic!


Git shortlog
............

Alex Popov (1):

    * Fixes zotonic/zotonic#1027 issue with zotonic-module script

Arjan Scherpenisse (1):

    *  mod_admin: Fix media upload when no content groups configured

Arthur Clemens (41):

    * mod_editor_tinymce: load the correct lib files
    * mod_admin: improve reusability of connections interface
    * mod_admin: fix background color of top menu on small screen
    * Make fswatch work with spaces in paths
    * mod_admin: prevent disconnecting when pressing enter
    * mod_admin: optimize display of connection list
    * doc: doc tweak
    * mod_admin: tweaks
    * mod_search: return multiple results for search by id
    * mod_admin: improve layout of thumbnails in find dialog
    * mod_admin: find dialog: visually distinguish existing connections for current predicate
    * mod_admin: align connection dialogs to browser top
    * mod_admin: remove redundant intro text
    * mod_admin: find dialog: visually distinguish existing connections for current predicate
    * mod_base: safer defaults
    * mod_base: safer defaults: add type "submit" to submitting buttons
    * mod_base: add bootstrap classname to invalid form
    * mod_editor_tinymce: link to correct codemirror
    * mod_editor_tinymce: use bootstrap classnames
    * mod_admin: admin_connect_select: fix actions in cases the value undefined is passed from a template
    * docs: refactor custom admin edit page
    * revert: template comment not allowed
    * mod_admin: remove margin from empty tree list
    * Revert "mod_base: safer defaults"
    * Revert "mod_base: safer defaults: add type "submit" to submitting buttons"
    * doc: describe "s" formatting character
    * mod_menu: allow nested divs inside the list item
    * mod_admin: make labels in list items visible
    * mod_admin: prevent emptied tree list taking up space
    * mod_menu: position all dialogs at the top
    * mod_admin: show default text when no features are enabled
    * zotonic_status: fix error message
    * doc: update zotonic_status site screenshots
    * mod_admin: tweak colors
    * mod_admin: tweak panel heading
    * mod_admin: align variables with bootstrap config
    * mod_admin: optimize layout of date fields
    * mod_admin: tweak colors
    * mod_admin: make growl messages less dark
    * mod_editor_tinymce: update codemirror plugin
    * mod_editor_tinymce: replace codemirror plugin with default code editor

Maas-Maarten Zeeman (3):

    * core: Fix for using template rendered file requests
    * core: Fix for uncollapsing lib files without a dirname
    * mod_component: Refactor of component injection code

Marc Worrell (55):

    * mod_email_status: styling of some buttons.
    * core: better error returns for m_identity:set_username_pw/4
    * mod_backup: fix periodic backup - broke because of added 'is full backup' flag in the backup list.
    * Fix a problem where the connected depictions were not always updated when adding a depiction.
    * Make the admin 'replace media' buttons an anchor element instead of a button.
    * Ensure that newly created connected content is in the same content group as the subject of the connection.
    * core: new pivot task return: {delay, Delay, NewArgs}
    * mod_filestore: changes to the 'move to local/remote' queries. This fixes an issue where moving many files at the same time results in timeouts during move initialization.
    * core: refactor the template compiler, use separate compilation process so that 'changed' checks can continue whilst compiling.
    * mod_acl_user_groups: add type="submit" to rule-add button
    * core: fix return value check of password change.
    * mod_acl_user_groups: set the default user group for user 1 and sudo to the adminstrators-group. Fixes #1042
    * mod_admin: fix a problem with uploading media if no subject_id is present
    * mod_acl_user_groups: fix default manager-group membership for admin or sudo
    * mod_contact: add form-group class to divs, this fixes the problem that validation errors were not flagged. Thanks to @fredpook
    * mod_search: add query term 'match_objects'.  This is similar to the {match_objects id=...} query.
    * core: if postback is handled, also return the page message queue.
    * mod_base: fix fetching queued transport messages
    * mod_translation: add config 'mod_translation.force_default' to prefer the default language above the browser accept languages.
    * acl: add option to check permission as-if the typical member has been logged in. Issue #1050
    * core: better error message in z_notifier.
    * core: fix find_value error in m_acl. Fixes #1052
    * mod_acl_user_groups: merge fix for sudo user groups.
    * core: fix a problem where the authoritatve uri could be requested by the datamodel installer before the id dispatch rule was known.
    * core: change stemming of the full text indexes.
    * core: fix a problem with abs_url and urls starting with '//'. This now correctly adds the protocol.
    * core: Stop email transmission if sender has been disabled or deleted. Fixes #1046
    * core: defined an admin as someone who is either user 1, sudo, or allowed to use the mod_admin_config. Fixes #1033
    * core: always return an error when looking up 'undefined' with name_to_id.
    * mod_search: fix a problem where search texts where stemmed twice.
    * mod_search: fix for dutch wildcard tsquery with dutch stemming; 'overstee' and 'oversteek' were mapped to 'overstee':* and 'overstek:*
    * erlydtl: hack fix for escape filter application in arg lists. Like '{foo bar=x|escape}'. For now direct mapping to the force_escape filter.
    * core: add dialogs and routines to move categories/content-groups/user-groups/predicates if one is deleted. Fixes #1041
    * mod_admin: fix js problem with touch js
    * mod_atom: fix test for new z_stdlib:strip
    * New locked version for z_stdlib and s3filez
    * mod_base: fix controller_comet for a problem where comet didn't flush on page data if a postback controller fetched the data before. (picked from d855b1abec4b55e0c0ab7c77f4a66c08da3194bd)
    * mod_base: export the post-loop in controller_comet.
    * mod_search: show growl error message on missing predicates and categories. Missing category matches the empty range. Fixes #998
    * mod_search: ignore 'undefined' categories in search.
    * mod_rest: fix a problem where a backup file could not be restored if the content group id was unknown. Fixes #1011
    * mod_search: fix fulltext query sql. Fix searching for prefixes which are wildcards. Fixes #1061
    * core: z_utils:is_empty/1, define St. Juttemis date as an empty value..
    * mod_base: add parameter less filter 'filter' to remove empty values from a list.
    * erlydtl: stricter definition of gb_trees, added lookup of m_search_result properties.
    * core: addes z_pivot_rsc/get_task/1,/2,/3 and /4. Used to check pivoted tasks
    * mod_admin: add filter temporary_rsc. Fixes #1044
    * docs: add some doc placeholders and documentation for the action mask_progress.
    * mod_video: add '-pix_fmt yuv420p' for QuickTime compatibility
    * mod_email_status: don't register a 'sender_disabled' error with the recipient's status.
    * core: expose pivot_location_lat, pivot_location_lng and pivot_geocode_qhash as m_rsc properties.
    * core: correct identify of MS Visio files.
    * core: allow 'gray' and 'grey' for the grey image filter.
    * core: set ``Cache-Control-Allow-Origin: *`` on all controller_file replies.
    * Lock deps. Fixes a problem where the empty url was changed into '/'. Fixes #1066

