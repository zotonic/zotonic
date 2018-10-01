.. _rel-0.41.0:

Release 0.41.0
==============

Welcome to Zotonic 0.41.0, released on 1 October, 2018.

Main changes are:

* Upgraded jquery from 1.11.1 to 3.3.1 (:issue:`1922`).
* Added livereload to mod_development (enable via ``/admin/development``).
* Fix a problem with default date handlng in resource updates (:issue:`1885`).
* mod_mailinglist now uses mod_email_status for bounce checks.

jQuery upgrade note:

Please note that if you manually include jquery-migrate in your
templates, you will have to change your templates manually because
jquery-migrate is also upgraded from jquery-migrate-1.2.1 to
jquery-migrate-1.4.1.


Commits since 0.40.0
--------------------

David de Boer (4):
      * core: Don't default start date to now (#1885)
      * core: Don't crash when files have been deleted (#1928)
      * docker: Fix rebar failure during Docker build (#1930)
      * core: Fix pivot failure during manage_data (#1932)

Maas-Maarten Zeeman (5):
      * core: Fix, minify all concatenated js and css files, not just the first
      * core: Compress svg files
      * mod_mqtt: Support retained messages
      * Added 0.41.0 release notes with an extra note about jquery migrate
      * core: Call z_datamodel:manage inside a transaction to prevent inconsistencies. See #1927

Marc Worrell (17):
      * mod_mailinglist: Fix typo in predicate name
      * mod_admin_merge: add option to add missing translations to the winner. (#1915)
      * mod_authentication: escape q arg
      * Fix leap year problem
      * Fix a problem with the slug when merging resources.
      * mod_translation: Fix a problem with copying languages
      * core: fix tz update for tables/columns that are reserved SQL keywords.
      * Use folsom and bear from github.com/folsom-project. Fixes #1920
      * mod_development: add livereload of css and js. Issue #1793 (#1924)
      * base: Upgraded old jquery to 3.3.1 (#1926)
      * Update mailinglist to work with email_status (#1929)
      * mod_development: work around for server caching of livereload css
      * core: Add option to use 'user_id' as controller_page id configuration.
      * base: allow 'user_id' as configured id for controller_redirect.
      * controller_file: fix a problem where IE11 could not download files.
      * Prepare 0.41.0
      * Fix issue for jquery.find('#'), upgrade bs js. Issue #1934 (#1935)

loetie (1):
      * Support failure_message in email_unique validator (#1931)
