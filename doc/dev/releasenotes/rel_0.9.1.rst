Release 0.9.1
=============

Released on 2013-03-14 19:31 by arjan.


Andreas Stenius (21):

* core: add support for 'optional include' templates.
* doc: Add `build:` tag to list of prefixes.
* doc: Add `translations:` tag to list of prefixes.
* doc: add links to other versions of the docs.
* doc: fix version links.
* doc: removed a tag from the version being browsed.
* doc: rename `translations:` to `translation:`.
* doc: rename link to 0.9
* doc: renamed sidebar template to be more generic.
* doc: update frontend-growl cookbook entry.
* mod_admin: optionally include country options.
* zotonic-tpl-mode: add test 10, to fix a regression.
* zotonic-tpl-mode: add test 8.
* zotonic-tpl-mode: add test 9.
* zotonic-tpl-mode: add test routines.
* zotonic-tpl-mode: add testsuite.
* zotonic-tpl-mode: bind C-M-\ to zotonic-tpl-indent-buffer.
* zotonic-tpl-mode: do not treat underscore as being part of a word.
* zotonic-tpl-mode: fix for indenting consequtive closing template tags.
* zotonic-tpl-mode: fix html tag highlighting when attr value contains //.
* zotonic-tpl-mode: indentation fixes.

Arjan Scherpenisse (49):

* Ensure iconv is started
* New bert.erl submodule
* New z_stdlib module
* basesite skel: Add show_media filter to home template
* build: Explicitly build lager first
* controller_api: Fix throwing a 404 error when service module not is found
* core: Fix use of hostname in startup / management scripts
* core: Include the request hostname in a dispatch rewrite notification
* core: Keep file extension when it is allowed for the file's mime type
* deps: Update mimetypes submodule
* deps: new gen_smtp
* doc: Add paragraph about multiple assignments to `with` tag docs
* doc: Add some documentation about startup environment variables
* doc: Document how to override TinyMCE options
* doc: Document mod_tkvstore
* doc: Document the "remove" action
* doc: Document the arguments of the "lazy" scomp
* doc: Explain API service naming in more detail
* doc: Explain the make_list filter to force model evaluation
* doc: Improve wording on replace filter
* doc: show_media filter: correct the default value for media dimensions
* mod_admin: Add button to run schema install from the site's module again
* mod_admin: Add inlinepopups TinyMCE plugin.
* mod_admin: Fix redirect to admin page when using admin logon form
* mod_admin: Fix some errors in new rsc dialog
* mod_admin: Fix view button class on readonly edit page
* mod_admin: Prevent id confict between forms
* mod_admin: clicking the "move" indicator of an edge should not open the details dialog
* mod_admin_config: Truncate large config values in the config table
* mod_admin_identity: Show inactive users in a lightgrey color
* mod_admin_predicate: Fix escaping of translated text in predicate help popup
* mod_base: Add z_stream_restart() function to zotonic-1.0.js
* mod_base: CSS typo
* mod_base: Fix 'width' argument for Bootstrap modal
* mod_base: Fix remember password for Chrome
* mod_base: Fix rendering issue in TinyMCE when scrolling
* mod_base: Log error in widgetmanager when evaluating the JSON data attribute fails
* mod_base: Remove accidental console.log
* mod_base: make controller_page:get_id/1 return undefined when no id is found
* mod_signup: Add fold notification for determining which fields to validate
* mod_signup: Allow signups with email only
* mod_survey: New question type "multiple choice"
* mod_translation: Add zh to languages list, add Shuyu Wang to translators
* mod_translation: Make checkbox for setting language as part of the URL
* mod_twitter: Change URL where one can resolve a Twitter ID
* scripts: Fix typo in zotonic-update
* scripts: When zotonic is not running, echo the output of erl_call
* translation: Add complete Chinese(zh) translation
* z_media_identify: Fix typo in re-run command on identify fail

Feather Andelf (1):

* translations: Chinese(zh) translations by Shuyu Wang aka andelf

Grzegorz Junka (2):

* Update controller_static_pages documentation
* skel: Update default nodb skeleton config

Ilyas Gasanov (1):

* translation: Added more complete Russian translations

Maas-Maarten Zeeman (1):

* Check if ua_classifier is functioning at startup.

Marc Worrell (57):

* core: add option 'use_absolute_url' to args and context of url generation (pages and media)
* core: added 'runtime' option to 'include' tag. (Renamed from undocumented and legacy 'scomp' argument.)
* core: added option to force the protocol of user facing urls. This is useful when a proxy translates between https and http.
* core: added z:ld(modulename), z:restart(sitename) and z:flush(sitename)
* core: added z_render:update_iframe/3. Used to render a template and place it into an iframe.
* core: allow an included file to use extends/overrules within the scope of the included file.
* core: allow anonymous access to the page_url_abs property.
* core: better error messages for z_db:q and q1 in case of sql errors.
* core: cache result of check if a specific module is enabled.
* core: ensure resource ids passed to the ACL are integers. This makes ACL modules simpler.
* core: fix for forced host/protocol redirects by mod_ssl
* core: fix return value of protocol redirect.
* core: fix spec of function.
* core: m_rsc - sanitize the uri input
* core: merges z_user_agent modifications from master (e3a6605ab3f9c48bda7eaa2d37c12bc7ad58a67b)
* core: more error messages instead of match error.
* core: prevent sites dispatcher from crashing when redirect page dispatch rule is missing.
* core: psql: better error message for insert/delete
* core: remove start/stop of iconv (is now eiconv)
* core: removed some ununsed var warnings.
* core: return 'undefined' for m_rsc:rid([]) lookups.
* core: show a stack trace when a periodic task crashes.
* core: use z_context:abs_url/1 when redirecting to the main site url. Also make it a permanent redirect (301)
* deps: add eiconv as git submodule.
* deps: new ua_classifier lib
* deps: new z_stdlib
* deps: new z_stdlib
* deps: new z_stdlib and new ua_classifier
* doc: add google analytics code.
* doc: added some missing smtp options
* doc: fix in docs for split_in and vsplit_in filters.
* eiconv: fix for warning on linux about missing iconv library.
* facebook/twitter: fix redirects, page_url is now a binary.
* mod_admin_identity: added #identity_verified{} notitication. Used to mark a verified identity to an user.
* mod_admin_identity: added a control to add/verify e-mail identities instead of the e-mail address input for a person.
* mod_admin_identity: fix for update_done notifications other than insert and update.
* mod_authentication: fixes for password reset e-mail and some error feedback messages.
* mod_base/core: added 'qarg' argument to postback actions. Enables adding the value of input elements as query args to the postback.
* mod_base: don't handle onsubmit validations for 'nosubmit' input elements.
* mod_base: moved the e-mail validation regexp to z_stdlib's z_email_utils.erl.
* mod_base: remove all zmodal masks when removing existing zmodals.
* mod_base_site: add style for meta share button
* mod_export: added generic export controller and notifications. (docs will be added)
* mod_l10n: added translations for relative time desciptions in the core z_datetime.erl.
* mod_search: allow nested category lists, allowing for more flexible category specification.
* mod_seo: fix for showing the summary when there is no seo description for the page.
* mod_seo: let search engines only index pages with real translation.
* mod_seo_sitemap: added category specific setting for update frequency and indexing priority.
* mod_seo_sitemap: fix for url generation, should page_url_abs.
* mod_seo_sitemap: generate language version links when mod_translation is enabled.
* mod_seo_sitemap: removed extra template, now uses catinclude.
* mod_seo_sitemap: use 'all catinclude' to fetch the extra seo options.
* mod_survey: added 'answers' option to survey_start, allowing answers to be preset (or added)
* mod_survey: fix for yes/no and true/false questions with immediate submit.
* modules: check if table is present before trying to create it.
* smtp: fix edocs parsing problem on binary notation in comments.
* smtp: use eiconv instead of iconv. Add better bounce recognition and is_bulk/is_auto flags for received e-mail.

Mgpld (1):

* mod_base: Modal html title in z.dialog

furiston (1):

* translations: Turkish translations by Evren Kutar aka furiston

