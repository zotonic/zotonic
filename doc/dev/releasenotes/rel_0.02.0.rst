Release 0.2.0
=============

Released on 2009-12-11.


New modules
-----------

mod_broadcast
   Send system messages to all users which are currently logged in in
   the Zotonic admin.
   
mod_calendar
   Shows event resources in a week-overview, and generates ICalendar
   feeds.

mod_mailinglist 
   Module which allows you to define groups of recipients and send
   mailings to them. Can also send via the unix sendmail program.

mod_twitter
   Receives feeds for Zotonic persons,using the Twitter streaming API.


* New core features:

"catinclude" and "all catinclude" tags 
   These include templates based on the category of a resource. Used
   in the admin to create custom fields based on category.
   http://zotonic.com/documentation/760/catinclude

Query search model
   Generate lists of resources on the fly. Used in mod_atom_feed to
   generate atom feeds, and has an API entrypoint, /api/search.
   http://zotonic.com/documentation/761/the-query-search-model

More template filters:
   in_future, in_past, rand, twitter, escape_ical

   
Bugfixes
--------

* Dynamic postgresql pool size, based on system load (issue #4)
* Issue in postgres pooling on stresstesting (#15)
* Uploaded files now get a proper mime type and extension (#5)
* And other issues: #2, #3, #9, #11, #14, #19, #20
