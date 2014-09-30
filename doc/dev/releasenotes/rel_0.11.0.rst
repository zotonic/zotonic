Release 0.11.0 (unreleased)
===========================

Timezones
---------

Timezone support was added to the core. All dates are now stored in UTC.
Resources with old dates are converted when read, assuming the configured server timezone.
You need to set the timezone in the ``zotonic.config`` file, for example:

    {timezone, "Europe/Berlin"}

A site-specific timezone can de set with the ``mod_l10n.timezone`` configuration.


Database driver and pool
------------------------

The database driver and pool has been replaced by standard epgsql and poolboy.
This removes the special Zotonic version of epgsql.

Updated modules
---------------

mod_l10n
  Added timezone support.

mod_development
  Added dispatch debugging and explanation.
  Added checkbox to disable the api-service ``/api/development/recompile``


New an updated filters
----------------------

date
    An optional second argument for the timezone has been added.

date_range
    An optional third argument for the timezone has been added.

truncate
	An optional second argument is added to specify the text added where
	the text is truncated.

truncate_html
	Truncates a HTML text to a specific length, ensures that all open
	tags are properly closed.


New notification
----------------

Added the notification ``request_context``. This is a foldl with the `Context` and is 
called after the request’s query arguments are parsed using ``z_context:ensure_qs/1``.
It can be used to perform transformations or actions based on the query arguments.


Misc
----

Rememberme cookie changes
    The *rememberme* cookie (used for automatic logon) is now based on a token instead of
    the user-id. The token is reset if the user’s password is changed.
    Cookies set using the previous scheme are invalidated.

User-defined Erlang dependencies
    It is now possible to add extra rebar ``deps`` to Zotonic, by
    adding them to the ``zotonic.config`` file.
    
