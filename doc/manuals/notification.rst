.. _manual-notification:

The notification system
=======================

Zotonic's notifier system makes it possible to create modular
components with pluggable interface. The notifier system is used by
internal core zotonic components like the authentication mechanism,
the logging system and more.

The notification system can not only act as a traditional event 
subscription system but also as an advance priority based function d
ispatch system. It uses the priority system which is used to select 
templates. This makes it possible to override pre-defined default 
behaviour of core zotonic modules.

A notification message is a tagged tuple. The first element of the
tuple is the type of notification message. An observer can use this 
to subscribe to specific messages it is interested in.

Example::

  {acl_logon, 1234}

Or::

  {module_activate, mod_stream, <0.32.0>}


Most of the notifications used in zotonic are defined as records and
can be found in ``include/zotononic_notifications.hrl``


Subscribe to events
===================

todo:: write something here. mention priorities. The observer_xxxx 


Sending events
==============

As mentioned earlier, the notification system can not only be used
to just send events to observers. Observers can also return values 
back. They can do this in various ways. 

Below you can find an overview of the methods of notification. 

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
  using the notication system it makes sure that modules are 
  decoupled. 
  
map
  Call all observers and get a list of all answers. 

foldl
  Do a fold over all observers, high prio observers first.

foldr
  Do a fold over all observers, low prio observers first.



