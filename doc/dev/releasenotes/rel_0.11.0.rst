Release 0.11.0
==============

Welcome Zotonic 0.11.0, released on October 8, 2014. These notes list
the most important changes for this new feature release.

.. note:: For upgrading to this release from an older Zotonic release, please
          read the :ref:`upgrade-notes` carefully, as some backward-incompatible
          changes have been made.


Full timezone support
---------------------

Timezone support was added to the core. All dates are now stored in
UTC.  Existing resources with old dates (in local time!) are converted
on read, assuming the configured server timezone.  You need to set the
timezone in the ``zotonic.config`` file, for example::

    {timezone, "Europe/Berlin"}

A site-specific timezone can de set with the ``mod_l10n.timezone`` configuration.


Database driver and pool
------------------------

The database driver and pool has been replaced by the standard
``epgsql`` and ``poolboy`` Erlang applications.  This means we no
longer use a special forked Zotonic version of the epgsql Postgres
driver. As a nice side effect, database queries run 5-10% faster on
the new database code. See the test report here:
https://gist.github.com/arjan/70e5ec0ecaf98b19a348


Global changes
..............

The default place for user-defined sites and external modules has been
changed to the defaults ``user/sites`` and ``user/modules``,
respectively.

Also, the global file ``priv/config`` has been obsoleted in place of a new
global configuration file, ``~/.zotonic/zotonic.config``.  Zotonic
actually looks in several places for its global configuration file,
including ``/etc/zotonic/zotonic.config``. See
:ref:`manual-configuration` for all information on this topic.


Updated modules
---------------

mod_l10n
  Added timezone support.

mod_development
  Added dispatch debugging and explanation.
  Added checkbox to disable the api-service ``/api/development/recompile``

mod_mqtt
  The :ref:`scomp-live` custom tag was added to :ref:`mod_mqtt`,
  allowing you to live update a part of the page on pubsub events.
  Documentation for :ref:`mod_mqtt` has been improved and a debugging
  option has been added so you can see which topics are published and
  subscribed to.

mod_base
  The Zotonic logo is now included in the distribution as a Webfont,
  resulting in crisp logo's in the admin and on the default Zotonic
  status website.

  
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


New transport mechanism
-----------------------

The client-server communication is now based on UBF-encoded
messages. It has become easier to send messages to specific pages
(browser tabs) and to do bi-directional communication in general. See
:ref:`manual-transport` for all information on this topic.


Misc
----

User-defined Erlang dependencies
    It is now possible to add extra rebar ``deps`` to Zotonic, by
    adding them to the ``zotonic.config`` file.

Version-locking of dependent Erlang applications
    Zotonic now uses the Rebar ``lock-deps`` plugin to keep all
    included dependencies at the versions that they were at when
    Zotonic was released. This improves the longterm stability of the
    Zotonic release.

Rememberme cookie changes
    The *rememberme* cookie (used for automatic logon) is now based on a token instead of
    the user id. The token is reset if the user’s password is changed.
    Cookies set using the previous scheme are invalidated.

Reuqest context notification
    Added the notification ``request_context``. This is a foldl with
    the `Context` and is called after the request’s query arguments
    are parsed using ``z_context:ensure_qs/1``.  It can be used to
    perform transformations or actions based on the query arguments.


Contributors
------------

The following people were involved in this release:

Alberto López, Arjan Scherpenisse, Arthur Clemens, David de Boer, Jeff
Bell, jult, Maas-Maarten Zeeman, Marc Worrell, Mawuli Adzaku and
Stephan Herzog.
