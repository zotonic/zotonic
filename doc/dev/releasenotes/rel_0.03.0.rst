Release 0.3.0
=============

Released on 2010-01-25.


New modules
-----------

mod_comment
   Enables a simple commenting system on your site using mod_comment.


New core features
-----------------

A new default site
   The default site of a vanilla Zotonic install is now modelled after
   a simple blog-style website, complete with an archive section,
   keywords, navigation to previous and next posts, atom feeds and
   comments.

Speed improvements
   The Webmachine code was restructured to be more lean-and-mean,
   yielding up to 20% more performance on page requests.

WebSockets support
   When WebSockets is available in the browser, then it is used as a
   replacement for the Comet long poll. Currently only Google Chrome
   supports this feature but it is expected to arrive in other
   browsers soon.

Admin updates
   Support for editing a location (Google map picker), a new
   collection type "query" was added for creating "saved searches".

EUnit support
   A start has been made to put the core functionality of Zotonic in
   unit tests using the EUnit testing framework. As of yet, only a
   small fraction of the code has been covered, but we'll keep working
   on increasing the code coverage of the tests.

   
Bugfixes
--------

* Resizing animated GIFs (#28)
* Determining EXIF orientation for images (#27)
* The OAuth API key management interface is now available from the admin. (#35)
* Hiding "meta" pages from the admin overview (#12)
* And dozens of small fixes which did not go through the issue tracker.
