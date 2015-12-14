.. _rel-0.13.6:

Release 0.13.6
==============

Welcome Zotonic 0.13.6, released on December 14, 2015.

This is a maintenance release of Zotonic 0.13

Main changes are:

 * Fix the deps version lock by adding USE_REBAR_LOCKED
 * New bin/zotonic command to start without a shell: *start-nodaemon*
 * Overview of edges in the mod_admin
 * New module mod_admin_merge to merge resources
 * Retained messages for ~pagesession topics
 * Performance fixes for api calls
 * Significant better revision history in mod_backup
 * Add ``no_touch`` option to ``m_rsc:update/4``
 * Many smaller fixes

The following 0.13.7 release will incorporate some performance and stability
enhancements from the master (1.0-dev) branch.

Commits since 0.13.5
--------------------

There were 96 commits since release 0.13.5.

Big thanks to all people contributing to Zotonic!


Git shortlog
............

Arjan Scherpenisse (1):
    * core: Revert edown revision bump due to compilation breakage

Arthur Clemens (24):
    * Tweak input border border to match buttons
    * doc: typo
    * mod_admin_predicate: fine-tune display of items
    * mod_signup: update translation strings, update Dutch
    * mod_authentication: update translation strings; update Dutch
    * doc: move number formatting to strings
    * doc: update examples so that they will work
    * mod_admin: style disabled tabs
    * doc: fix make script
    * doc: fix make script
    * mod_admin: add category name to category dialog on edit page
    * mod_admin: add category preselection to connect-find dialog
    * filewatcher: handle spaces filepath in less command
    * filewatcher: handle spaces filepath in less command
    * mod_admin: reverse created/modified
    * Remove borders from connections list
    * mod_admin: align category param between find and new tabs
    * mod_translation: handle concatenated lists
    * mod_admin: generate translation strings, update NL
    * mod_acl_simple_roles: typo
    * mod_admin: add dependency mod_mqtt
    * doc: improve docs on CORS
    * mod_editor_tinymce: remove codemirror init code
    * mod_admin_merge: grammar and text tweaks

David de Boer (2):
    * Add modules model docs
    * Fix query export

Maas-Maarten Zeeman (6):
    * mod_mqtt: Added a way to pass js arguments for mqtt wires.
    * mod_base: Use beacon api to get good page close feedback for chrome/firefox and recent android users
    * mod_mqtt: Added retained messages for local (~pagesession) topics
    * mod_component: Updated mithril to version 0.0.2
    * mod_mqtt: Just return the topic if it is not a site topic
    * core: Make sure looking up translations always succeed. Fixes #1103

Marc Worrell (57):
    *  mod_acl_user_groups: fix acl check for module use rights. Fixes #1071
    *  Merge pull request #1064 from driebit/modules-model-doc
    *  Add USE_REBAR_LOCKED file to use the locked config
    *  Fix erlware_commons version, as the erlware_commons master requires rebar3
    *  Copy Travis config from master
    *  Lock version of new mochiweb and qdate
    *  mod_admin_predicate: add overview pages of edges, optionally filter by predicate. Issue #1075
    *  core: tuning the full text search.     Add option to exclude certain predicates from adding the object's title to the pivot text.     Set different full text ranking weights and behaviour
    *  mod_admin_predicate: rename 'connection' to 'page connection'
    *  mod_admin: add the 'all connections' button to the dashboard
    *  mod_search: fix for rank_behaviour arg in simple fullext query.
    *  mod_base: add convenience arg to lazy scomp. If 'template' argument is defined then all arguments are wrapped in an update action.
    *  core: add controller_http_error for handling HTTP errors in other controllers.
    *  Locked new webzmachine (needed for the error controller)
    *  mod_survey: add option to send answers to the respondent.
    *  mod_survey: change some texts to make options clearer.
    *  filewatcher: use os:find_exectutable/1 instead of os:cmd("which ..") to find command line executables. Fixes #1078
    *  core: check for the presence of 'identify', 'convert' and 'exif' before execution. This gives better error messages. Issue #1078
    *  core: fix 'undefined' warning if rsc is inserted with content_group_id set to 'undefined'.
    *  mod_admin_identity: let all people with use.mod_admin_identity manage usernames and passwords. Fixes #1077
    *  mod_acl_user_groups: refine 'block' semantics, now blocks fo the mentioned user group.
    *  Undo changes by commit c10055bdd581877d9df113407d8083877a1bc6b6
    *  erlydtl: change generated (image tag) code to never echo errors inline in the template. Instead call lager or error_logger.
    *  core: fix tag/viewer generation (typo)
    *  core: fix a problem in z_email_server with mailing attachments by rsc id.
    *  mod_base_site: add z.clickable.js
    *  mod_base_site: add email to the share page dialog.
    *  mod_admin: change 'replace media item' text to 'add media item' if no medium is present. Thanks to @fredpook.
    *  mod_admin: use live templates for the connection lists.
    *  core: fix order of subcategories returned by is_a
    *  core: cleanup text to pivot and search. Remove '-', '/', and tags. Unescape the remaining text.
    *  smtp: mark emails from the mailer-daemon as automatic emails.
    *  Merge changes by @CyBeRoni issue #1082
    *  mod_search: allow to check on 'is null' and 'is not null' using '='/'<>' and 'undefined'
    *  New z_stdlib.
    *  Lock parse_trans to newer version.
    *  Move to uwiger/jobs instead of esl/jobs
    *  New z_stdlib
    *  mod_admin: set 'inputmode' attributes. Remove 'type=url' to prevent Chrome from checking the input. Issue #1093
    *  mod_admin: allow changing edges on the 'linkable' permission instead of 'editable'. Fixes #1098
    *  mod_base: fix some performance regressions in controller_api and z_service. Add simple Techempower json benchmark.
    *  New webzmachine - fixes logging of 500 errors.
    *  mod_backup: redo user interface of revision history
    *  Merge pull request #1088 from driebit/fix-export-query
    *  core: add api to return edge properties. Add 'no_touch' option to m_rsc_update, and option to set creator_id and craeted on edge. Fixes #1087
    *  mod_acl_user_groups: add a 'published' check to SQL searches for non-admin users. Small cleanup in z_search. Fixes #1081
    *  core: remove sql errors when enabling modules in site (re)start.
    *  core: log errors returned from collecting the custom pivots.
    *  core: add z_pivot_rsc:pivot_delay/1, makes it possible to delay pivoting when performing many resource updates.
    *  core: trim pivot fields like name and city.
    *  core: new functionality to merge two resources. Adds module mod_admin_merge and #rsc_merge{} notification.
    *  docs: new placeholders for mod_admin_merge and some new controllers.
    *  core: fix rsc_gone lookup for new location
    *  core: when merging, also merge properties (todo: map languages).     Fix a problem in the admin new-rsc dialog when the category selection is set to '*'
    *  mod_admin(_merge): prevent form submit when pressing enter in the connect or merge search fields.
    *  mod_content_groups/mod_admin_category/mod_acl_user_groups: Delay pivoting whilst peforming updates of many resources.
    *  Prepare for 0.13.6 release, preliminary release notes.

Marco Wessel (5):
    *  Allow properly starting up in the foreground
    *  Script should be executable of course.
    *  Reinstate heart
    *  Reverse logic
    *  Be correct about quoting variables

loetie (1):
    *  Typo inside is_allowed check mod_import_csv.erl



