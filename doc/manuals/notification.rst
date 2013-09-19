.. _manual-notification:

The notification system
=======================

Zotonic’s notifier system makes it possible to create modular
components with pluggable interface. The notifier system is used by
internal core Zotonic components like the authentication mechanism,
the logging system and more.

The notification system can not only act as a traditional event
subscription system but also as an advance priority based function
dispatch system. It uses the priority system which is used to select
templates. This makes it possible to override pre-defined default
behaviour of core Zotonic modules.

A notification message is a tagged tuple. The first element of the
tuple is the type of notification message. An observer can use this 
to subscribe to specific messages it is interested in.

Example::

  {acl_logon, 1234}

Or::

  {module_activate, mod_stream, <0.32.0>}


Most of the notifications used in Zotonic are defined as records and
can be found in ``include/zotononic_notifications.hrl``

Sending notifications
---------------------

As mentioned earlier, the notification system can not only be used
to just send events to observers. Observers can also return values 
back. They can do this in various ways. 

Below you can find an overview of the notification methods. 

notify
  Send a message to all observers. This is used if you want to
  notify other observers about a specific event. In Zotonic this 
  is used a lot. For instance, it is used to notify modules of 
  about user logons, or notify when modules are activated and 
  deactivated.

notify1
  Notify the first observer. This is useful for if you want to
  be sure just one observer can do something with the message. 

first 
  Call all observers, and use the first non ``undefined`` answer.
  This is used to get information from one of the observers. By
  using the notification system it makes sure that modules are 
  decoupled. 
  
map
  Call all observers and get a list of all answers. 

foldl
  Do a fold over all observers, high prio observers first. 

foldr
  Do a fold over all observers, low prio observers first.

Subscribing to notifications
----------------------------

Registering as an observer works as follows:: 

   z_notifier:observe(NotificationType, Handler, Priority, Context)

If the module is either a Zotonic module or a site module, the 
``Priority`` parameter can be omitted. The observer will then get 
the same priority as the module.

NotificationType
  The type of notification you want to observe, an atom.

Handler 
  Can be a ``pid()``, or a ``{Module, Fun}`` tuple. When the handler
  is a ``pid()`` and the notification is sent with ``notify`` or ``notify1``
  the gen_server process receives a ``handle_cast``. When an answer is 
  expected back ``handle_call`` is used. This is the case for ``first``, 
  ``map``, ``foldr`` and ``foldr``.  

Priority
  The priority of the observer. This influences the order in which 
  they are called. 

Context
  The Zotonic context.

Example::

   z_notifier:observer(acl_logon, {mysitewww, handle_logon}, Context)

Subscription shorthands
.......................

Modules and sites can use shortcuts for registering as an observer. When the
Zotonic module exports a function with the prefix ``observe_`` or 
``pid_observe_`` Zotonic’s module manager will register the observer for you.

For example exporting ``observe_acl_logon/2`` will register that function as
an observer. It will be triggered when the ``acl_logon`` notification is fired.

Handling notifications
----------------------

When a notifications is sent the ``z_notifier`` module looks in its
tables to see if there are any observers interested in receiving
it. There are three types of notifications.

Cast notifications
  This is the simplest notification. The notifier does not expect an answer back
  the result of the handler is ignored. This kind of notification is triggered by
  calling ``z_notifier:notify/2`` or ``z_notifier:notify1/2``. They are useful
  for letting other modules know about a certain even or condition. This 
  makes it possible for other modules to act on it.

  For example, :ref:`mod_development` uses call notifications to trigger builds
  and reloads. By doing this other modules can notify ``mod_development`` to 
  trigger builds. But when ``mod_development`` is disabled nothing will happen.

Call notification
  For this kind of notification, ``z_notifier`` expects an answer back. This answer
  is returned back to the notifier. This kind of notifications is used to 
  decouple modules. For instance a module can ask another module for a special
  URL to go to after logging in without knowing which module will do this. 
  Call notifications are triggered by: ``z_notifier:first/2`` and 
  ``z_notifier:map/2``.

  For example, :ref:`mod_signup` uses a call notification to find out what page
  to redirect to after a successfull signup. This allows one to customize the 
  signup process.

Fold notifications

  Fold notifications are called, with ``z_notifier:foldl/3`` or
  ``z_notifier:foldr/3``. It works similar to the `lists:foldr and
  lists:foldl <http://www.erlang.org/doc/man/lists.html#foldl-3>`_
  functions of Erlang’s `lists
  <http://www.erlang.org/doc/man/lists.html>`_ module.

  The fold function calls each observer in sequence, either starting
  at highest (``foldl``) or at lowest (``foldr``) priority, passing
  values and an initial accumulator value.

  Each observer can adapt values in the accumulator, and needs to
  return it, for passing on to the next observer. The final value of
  the accumulator is returned as result. This is useful if you want
  multiple modules to be able to adapt and use values in the
  accumulator.

  For example, :ref:`mod_admin` uses a fold notification (called
  ``admin_menu``) to build up the admin navigation menu, where each
  observer is called to add menu entries to the menu.
