.. _manual-cli-shell:

The Zotonic shell
=================

The Zotonic shell gives you access to a running Zotonic instance with its code and data.

To connect to the background Zotonic server instance within an EShell (Erlang shell)::

  zotonic shell

Alternatively, use the `debug` commmand to launch the Zotonic server interactively and get an EShell on the running instance::

  zotonic debug

The ``start.sh`` command in the root folder is a shortcut for this command.


Tab completion
--------------

The Zotonic shell has tab completion.

``z_<tab>``
  Lists Zotonic library modules.

``m_<tab>``
  Lists Zotonic models.

``mod_<tab>``
  Lists Zotonic modules.

Add a colon to get all available functions from a module, for instance: ``m_rsc:<tab>``.


Often used commands
-------------------

The ``z`` module provides shortcuts for the most used commands. From within the Zotonic shell:

``z:m().``
  (Re)makes all erlang source modules and resets the caches.
  
``z:flush(sitename).`` or ``z:flush().``
  Resets all caches, reloads the dispatch rules and rescans all modules.

``z:ld(modulename).`` or ``z:ld().``
  Reloads an Erlang module, or reloads all changed Erlang modules
  
``z:restart(sitename).`` or ``z:restart().``
  Restarts the site, or restarts Zotonic

``z:c(sitename).``
  Gets the current site context. This call is often used inside another function call - see examples below.


Activating/deactivating modules
-------------------------------

``z_module_manager:deactivate(mod_modulename, z:c(sitename)).``
  Deactivates a module.

``z_module_manager:activate(mod_modulename, z:c(sitename)).``
  Activates a module.


Resetting a user password
-------------------------

Sometimes it happens that you want to reset an user's password from the Erlang shell. You can do this from the Erlang shell without using the admin or the reset-password mail/dialog.

``m_identity:set_username_pw(1234, "username", "password", z:c(sitename)).``
  Where 1234 is the rsc id of your user (1 for the admin), this must be an integer.


Accessing data
--------------

Sometimes it is desirable to access data stored in Zotonic from the EShell. This is useful for experimenting with code without having to go through an involved process or through HTTP requests to test it.

These calls use the site context as parameter. Get it using::

  Context = z:c(sitename).

The ``m_rsc`` model module provides functions for retrieving and interacting with pages and other resources stored in Zotonic’s datastore for the site.

``RscProps = m_rsc:get(Id, Context).``
  Returns the entire resource as a proplists structure. Id is the resource Id number.

``Title = m_rsc:p(Id, title, Context).``

You can also use the Erlang proplists module:

``Title = proplists:get_value(title, RscProps).``
 


Searching
---------

To perform a search on the running site, use the ``z_search`` module.

``rr(z_search).``
  Loads all records defined in ``z_search`` (including those defined in include files such as the ``#search_result`` record).

``Results = z_search:search({latest, [{cat, text}]}, Context).``
  Returns the search result record, for the search on pages from the category `text`. You can specify the category as a string, binary, atom or integer.

To retrieve the list of pages, we access the ``result`` property of the record data:

``Pages = Results#search_result.result.``

Use ``m_rsc:get(Id, Context)`` to retrieve Page information of each search result (see above).



.. highlight:: bash


