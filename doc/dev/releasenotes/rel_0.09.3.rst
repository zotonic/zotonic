Release 0.9.3
=============

Released on 2013-09-21 09:37 by arjan.

The main focus of this release is to make it compatible with the
Erlang R16 series. Other changes are below, grouped by developer.


Arjan Scherpenisse (24):

* core: Fix .ogg/.ogv to be correctly identified as audio/video.
* core: Implement "center of gravity" option for crops
* doc: Fix syntax error in generated sphinx conf.py file
* doc: Move the section on git commit messages to the "contributing source code" section.
* mod_acl_simple_roles: Fix typo
* mod_admin: Create interface to pick the cropping center on images.
* mod_admin: Do foldr over the blocks instead of foldl
* mod_admin: Speed up admin edit page JS performance
* mod_admin_identity: Also offer the "send welcome" option when changing the username / password
* mod_admin_predicate: Fix category tree for fluid layout change
* mod_base: Add optional tabindex to button tag.
* mod_development: Fix crashing file watcher process while editing templates
* mod_oembed: Better error reporting and "fix all" button
* mod_oembed: Fix setting media title when programmatically importing an oembed media item.
* mod_oembed: Prevent crash in media tag when no oembed metadata is found.
* mod_survey: Fix crashing survey results
* mod_survey: Improvements (placeholder texts for input fields, translateable validation msgs)
* mod_twitter: Fix Twitter logon and API access for changed 1.1 API

Marc Worrell (21):

* core: added 'magick' image filter options. Fixed mime lookup if lossless filter was inside a mediaclass definition.
* core: added z:ld/0 to load all re-compiled beam files.
* core: filenames might have a space, escape it befor os:cmd/1
* core: fixed mediaclass lookup - now all lookups are done as documented with adherence to the module priority and the ua lookup.
* core: move rsc property access check inside  p/3 instead of the model access.
* core: remove debug statement from z_acl.
* core: removed debug messages from z_media_tag.
* deps: newest z_stdlib.
* m_identity: ensure that non-unique identities have is_unique set to 'undefined' (instead of false)
* m_media: accept upload of 'data:' urls for replace_url and insert_url functions.
* mod_admin: fix replacing media items.
* mod_authentication: send e-mails to all matching accounts, if and only if a username is set.
* mod_authenticaton: fix input sizes for password, and e-mail/username fields.
* mod_base: added 'is_result_render' option to 'moreresults' action. Allows render of all results at once instead of per result item.
* mod_base: added touch punch to enable drag/drop and other touch events on mobile touch devices. Fixes #597
* mod_base: fix hybi00 websocket connections. Fixes #637
* mod_base: fix in LiveValidation for resetting error class on textareas. Fixes #601
* mod_base_site: add class 'wrapper' for content, allows for better styling. Hide breadcrumb and subnav when the current page is the only page in the breadcrumb or subnav.
* mod_geomap: Perform country lookups by iso code - works better with OpenStreetMaps API.
* mod_geomap: export lookup functions, add hard coded lookups for exceptions.
* mod_geomap: load cluster popups via template. Disable google geo lookups for half an hour if quota exceeded. Cleanup of some js code. Perform geo lookups in same process as pivot, prevents too many parallel lookups when re-pivoting.

