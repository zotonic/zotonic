Zotonic 0.8.2
=============

Released on 2012-10-30 11:48 by arjan.


Arjan Scherpenisse (3):

* Full text search: use the default postgres tsearch catalogs.
* Add new webzmachine commit for Firefox Websockets fix.
* mod_import_wordpress: Support schema 1.2; fix XMerl unicode behaviour.

Artur Wilniewczyc (1):

* fix multipart form data parsing: add new data at the end of previous data

Marc Worrell (8):

* Fix vulnerability in the random number and id generation. Fixes #369
* Fix mixup of tr/pl iso codes. With thanks to Kunthar.
* Allow searching on texts like 'id:1234' to find a specific id.
* Allow query_id to be a page with 'haspart' objects. Don't throw
  exceptions on mismatch of categories (this allows a query to be
  inserted before the referred categories are inserted)
* Only make queries with the 'is_query_live' flag a live query. Also
  some extra updates to m_rsc_update for later REST API work. Fixes
  #344
* Also merge the sort terms from 'query_id' includes.
* Add 'publication_before/after' search terms. Fixes issue #361
* Fix query for haspart of a collection.

