Release 0.8.1
=============

Released on 2012-08-11 16:36 by arjan.


Ahmed Al-Saadi (1):

* Removed websocket response header to conform to the latest proposed websocket protocol standard 
  (see http://tools.ietf.org/html/rfc6455#section-4.2.2, December 2011)

Arjan Scherpenisse (20):

* Fixed the "make docs" command in the Makefile
* Fix 2 typos.
* mod_mailinglist - depend on mod_logging.
* mod_mailinglist - address recipients by name in welcome/confirm/goodbye messages.
* mod_mailinglist: new filter to add recipient details to links in mails.
* mod_backup: Prefix backup files with the site's name.
* Dynamically calculate the ebin dirs when compiling .erl, to ensure deps/ ebin dir existence.
* Fix wording of SEO webmaster tools configuration. Fixes #310.
* Merge remote-tracking branch 'origin/release-0.8.x' into release-0.8.x
* Added optional argument to show_media filter which specifies the used template for media items.
* z_email_server: Force quoted-printable encoding on HTML and Text parts in emails.
* Show e-mail addresses that have been entered in the survey in an easy copy-pastable dialog.
* mod_survey: Added easily-printable output.
* mod_survey: Sort the printable list on name_surname by default. Sort argument from the query string is also supported.
* mod_survey: Use lowercase when sorting printable list
* Backport fixes to Twitter filter to 0.8.
* Fix blog skel indentation. Fixes #329

Maas-Maarten Zeeman (4):

* The session id was not stored when a new session was created. Hopefully this fixes #327.
* Fix for mochiweb:to_html bug with singleton tags. Fixes issue #328
* Make language dependent URL rewriting configurable. Fixes #315
* Fix for logoff option. Fixes #382

Marc Worrell (8):

* Fixed a problem when editing a newly added menu. This was already fixed in the new-admin-design branch.
* Fixed typo in attr name ('check' instead of 'checked')
* Fix cache key of page. Issue #313
* Added travis-ci config from master.
* mod_survey: Added some css to display survey questions.
* mod_survey: Better likert display
* mod_survey: Better matching display
* mod_survey: Better longanswer display
