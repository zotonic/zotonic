Release 0.6.1
=============

Released on 2011-07-06.

Konstantin Nikiforov

* jquery actions insert_before, after, replace


Atilla Erdődi

* Activity Streams
* added notification for comments

Arjan Scherpenisse

* Mailinglist - fix database updater
* Fixed zotonic-stop command by using -name instead of -sname
* Services API tweaks
* Made group_title_firstchar filter work with translatable content.
* Fix up base/info service -- add translation lookup to administrator title.
* Small tweak to atom entries, better follow atom spec
* Remove link to atom_feed from mod_base's base template to remove dependency on mod_atom_feed.
* Fix lib tag for including multiple files directly from the toplevel 'lib' folder.
* Release depcache locks immediately after exception in z:memo().
* Use translation lookup in internal link action.
* Make dispatch rule reloading more stable.
* Make z_utils:url_decode/1 work and add unit tests.
* z_media_tag: Ensure that the generated URL is a binary, even if it's empty.
* Fix for editing content which used to be multilingual but is no longer multilingual.
* backport changeset 05a5254b6c92 - Fix for embedded media not showing up
* Fix translation statistics for modules in places other than modules/
* Fix month/day name capitalization: dutch does not do capitalization.
* Fix TinyMCE double initialization for the rsc-body.
* Fix unnecessary sharpen in lossless (PNG) images.
* Backported fix for issue 158 to 0.6 (twitter images)
* Fix solr searching because z_convert:to_isotime now returns a binary.
* Allow for recursive file lookups in z_module_indexer:scan_subdir/4
* Fix menu fetching on 0.6; update docs
* Fixed essential typo in mailinglist module.
* Only use gzip encoding for mime types that actually benefit from it.
* Compile the Zotonic behaviours before the other source files.
* Fix mailinglist upload button for changed #upload{} record.
* Fixed a bug in mod_acl_simple_roles which caused a NULL violation when setting visible_for.
* Make zotonic-1.0.js compatible with jQuery 1.5
* Remove string reversal from mod_logging which was previously needed.
* fieldreplace: Removed toggle class from click handler, because change handler is also triggered.

Marc Worrell

* Show the 'voorvoegsel' field for nl, de languages.
* Add identify/2 for #upload records.
* Small style fix for non-translation editing of the body.
* Added support for subscribing persons in the database to a mailinglist.
* Added flipchart mime type.
* Fixes for looking up flipchart, upper case extensions and mime icons.
* Fix for getValue on an empty select element.
* Make pivoting dates more resilient against illegal dates.
* Fetch the widget options from data-... attributes instead from in the element's class.  The class is still parsed for backwards compatibility.
* Move widget options to data-... attributes.
* Remove newlines before parsing the widget options.
* always call the widgetManager when inserting content.
* Added 'visible' option, triggers the moreresults action on visible of the element instead of on click.
* Added z_comet to the list of zotonic query args.
* Added 'if' filter.
* Added missing mailinglist footer template.
* Changes in the acl_add_sql_check notification, now uses a record, including the search sql.
* Added visibility toggle to mod_comment.
* Added support for generic msoffice mime type returned by 'file'
* Fix for resetting the textarea after committing the comment.
* Clear textarea and input fields after a comment has been posted.
* Added randomize filter, shuffles a list.
* Added 'matching' type of question and results view for select in 'narrative'
* Wired the 'postback_notify' event directly into the postback/websockets resources. This makes javascript triggered postbacks more flexible and simpler.
* Allow data_xxxx attributes, which map to html5 'data-xxxx' attributes.
* Added 'do_slideshow' widget. Needs a data-slideshow with the id of the elements to be shown in a slideshow dialog.
* Added cursor property for do_slideshow elements.
* Fix for validator, empty string should pass the format validation.
* Fix for black background when previewing transparent images as jpeg.
* Added form reset action.  Fix for submit/reset of 'closest' form. Fix for IE problem with inputoverlay.
* Let do_inputoverlay also work for textareas
* Added 'propagate' option to event action (wire scomp). This prevents canceling the event after being handled.
* Added created column to overview.
* Better embedding of images. Now fetches the image using the rights of the sender.  Also initialises the random number generator for the inline image cids.
* Removed content disposition of 'inline' images.
* Add X-Attachment-Id to embedded images.
* Added plain text alternative to sent e-mails.  Based on markdown text of the html.
* Added 'acl' option, similar to the acl option of the resource_template.
* Better feedback when a validation fails on form submit.
* Added 'is_public' and 'ongoing' query terms.
* Added z_template:render/2 to render a #render{} record.
* Added poll, including poll templates.  A poll should have a start/end date, used for displaying.
* More subtle error message when form does not validate.
* Fix for a problem where the substr of translated binaries was taken.
* Fix for binary format string.
* Fix for binary results from translations.
* Made do_listfilter more generic.
* Make erlydtl_dateformat a binary. Correct handling of binaries or lists for day/month representations.  Re-enable test set.
* Allow institutions to have an username/password.
* Show date range for surveys.  Always show poll results.
* Let the 'remeber me' be valid for 10 years or so instead of two weeks.
* Fix for max_age of persist cookie
* Updated modernizr, fixes hanging bug in Chrome.
* Added survey result download in CSV (tab delimited) format.
* Allow to specify 'propagate' with the wire scomp
* Added download link to survey results. Also changed tab delimited results file to comma delimited file.
* Hide results when user is not allowed to see the results.
* Fix for editing multilingual content with or without mod_translation.
* Added possibility to have 'undefined' array values.
* Log an error when we run into a compile error.
* Simple expression evaluator, makes use of the erlydtl parser and runtimes.
* Added conditional jumps on page breaks. Added missing license texts.
* Added fix for 'file' utility identification error on mpeg files.
* Added l10n module for further localization. Currently implements a list of languages by iso code with translations.
* Added a 'whereis' api to z_module_manager to find the pid of a running module.
* Fix for mod_l10n description.
* Fixes for pivoting country names.  Expands iso code to country names in languages as defined in the page.  Only use the english postgresql indexing to prevent problems when searching in other languages.
* Map upper case iso to lower case for pivoting.
* Workaround for validations of elements that are removed.  Needs better solution.
* Fixes for dynamic changes in LiveValidation checked forms. Keep the administration of LiveValidation objects in the data() of the form elements.  So that you can dynamically add/remove elements without the LiveValidationForm object getting out of sync.
* Prevent retry loop when providing an illegal page id
* Move the init of non-gen_server modules to the dummy module process instead of the module manager's process.
* Added user_from_page/1 and user_from_session/1 to derive the user id from a page or session pid.
* Fix for a problem where the 'appear' was called on the enclosing container for insert_top/bottom instead of the inserted html fragment.
* Part of previous commit to fetch user_id from page pid.
* Added do_popupwindow widget to open a link in a popup window.
* Replace textual smiley codes with images.
* Supervisor for named processes, based on Erlang's supervisor.erl
* Added firefox smiley.
* Fix for a problem where widgetManager is only called for the first element in the jquery context.
* Fix for popupwindow namespace.
* Fixes for window name on IE, and fixes for correcting size of popup after opening window.
* Delayed size correction, Chrome returns 0 when the window is just opened.
* Added support for anchors and tables to markdown translators.
* Added no_html option to html2markdown filter.
* Make it possible to pass additional variables to the rendered result template.
* Use the no_html option when translating html to plain text.
* Fix for z_render:set_value/3
* Allow property names as string values.
* Added cc to email record.
* Added support for Cc
* let os_escape of undefined return []
* Fix translation table references when pivoting resources.
* Added 'full' option for popup windows.
* Added live updating do_timesince. Added z_translate function, uses a lookup object to find translations.  Fixed local/utc time problem in 'U' date format.  Fixed copyright name in smiley.
* Fix for site/hostname mixup.
* Fix for opening a window in 'full screen' size.
* Echo q.username so that we can have direct link to the password reminder when we know the username.
* Added version to activity record.
* Added missing survey_results resource.
* Added notifications on edge insert/delete.  Incomplete but good enough.
* Made session timeout configurable, use site.session_expire_1 and site.session_expire_n
* Automatically cleanup temporary files when the request process is stopped.
* Allow more than one do_ widget per element.
* Fix for resizing images embedded in e-mails.
* Added Google Analytics _trackEvent do_gaq_track
* Added m.comment.get for a comment_id
* Added reload of pages on logon or logoff - first part of smarter session management and open pages.
* Let the reload action take into account the continuation page of a logon screen.
* Merging 0.7 email additions to help with site transitions from 0.6 to 0.7
* Added to_flatlist/1
* Added 'attachments' field to email record, for easing upgrading 0.6 sites to 0.7.
* Added newer jquery and jquery ui to fix drag problems in IE9
* Remove <style> tags when converting html to markdown.

