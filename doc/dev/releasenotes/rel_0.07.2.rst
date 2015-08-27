Release 0.7.2
=============

Released on 2011-12-11 19:51 by arjan.


Alain O'Dea (1):

* Ignore compile output and runtime generated files

  
Arjan Scherpenisse (mod_mailinglist):
  
* Added new bounce handling dialog.
* Attach documents to the mailing for each 'hasdocument' edge instead of 'document'.     
* Added administration of worker processes to email server.
* Updated translations, removed references to mod_emailer.     
* Fixed using the configured address in mailinglist rsc as the "from" address.     
* Re-added the option of test-sending a mailinglist page to a single address.
* Removed already_sent check from mod_mailinglist, which is not needed since the new interface.     
* Fix include reference to mailing footer template.     


Arjan Scherpenisse (mod_survey):

* mod_survey: made questions configurably required or not.
* Added dutch translation for mod_survey; small template tweaks. Fixes #205     
* mod_survey: quick hack to put email validation on a field if you name it 'email'.     
 

Arjan Scherpenisse (misc):

* Added support for varying overview lists in the admin on category.
* mod_facebook: make 'scope' parameter configurable.
* On win32, mime type returned as application/octet for all files.
* Added spanish translation and install the spanish language by default and enable it.
* Added a file with the translators per language.
* Scan all erlang files in the modules for translatable strings.
* Added missing file
* Added 2 new translation template files.
* Add check in zotonic-addsite to see if the given site name is valid.
* tiny_mce: Fixed the disappearing of inline images. Fixes issue #203.
* Tooltip Y position is now dependent on its height. Fixes issue #207     
* Disregard stderr output from identify command. Fixes issue #206     
* Make windows users happy when redirecting stderr.     
* Added dutch month/day names to mod_l10n.     
* z_datamodel: do not try to resolve 'undefined' in valid_for check     
* Generalized group_title_firstchar filter into group_firstchar.
* Added date_start_year= and date_end_year= search filters to filter on year of date start/end.     
* Fix infinite recursion in sub_month/3 filter.     
* mod_import_csv: Added import button to admin status page.
* m_rsc_update emptied the pivot_date_* fields when ``date_`` fields where not part of the update.
* Added application/x-font-woff for webfonts.     
* Allow modules to override admin TinyMCE options which were originally set in admin-common.js
* Addressed the issues in the backup module. Fixes #220
* mod_backup: make sure we have an archive dir before archiving.     
* Added option email_bounce_override to override bounce email address.
* Allow id to be either number or unique name for resource_admin_edit.
* Export z_session_manager:get_session_id/1.     
* z_html: do not escape/strip HTML when a property name ends in _html.
* z_html:escape_props/1 -  Make selecting escape function more safe.     
* Fixed picture rotation detection by tweaking the parser of the output of "exif -m -t Rotation".     
* Fixed z_utils:tempfile() to respect OS environment variables.
* z_convert:to_json/1 now also accepts floating point numbers.
* Fix SQL error in z_installer.
* Add distinctive icon for internal link in TinyMCE. Fixes #189     
* Removed m_identity:{get,set}_props which were unused and not working.
* Show language selector on admin media page. Fixes #253     
* mod_twitter: remove invalid {verbose, trace} option.     
* lower/upper filters now try to convert their argument to a list if they're not.     
