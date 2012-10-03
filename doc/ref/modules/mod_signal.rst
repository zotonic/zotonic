.. include:: meta-mod_signal.rst

`mod_signal` allows template developers to create pages with highly interactive behaviour. It allows one to 
emit so called signals. Signals are asynchronous events with attributes. Other pages can connect to the signature 
of a signal. When that signal is fired, the specified action is triggered. This makes it possible to create 
interactive pages without writing a line of erlang code.

Signals
=======

A signal is an erlang tuple which can be emitted either by erlang code::

  mod_signal:emit({example, [{test, "a"}, {session, 123}]}, Context)

or by using the emit action in a template::

  {% button action={emit signal={example test="a" session=123}} %}

Signals themselves don't do much. The fun part is connecting other stuff to signals. When a signal is 
emitted ``mod_signal`` examines if there is something which is interested in this particular signal. It could 
be that there are actions connected to signals of type ``example``. When such a signal is emitted the registered 
action is performed. This can be done with the ``connect`` action.

For example:: 

  {% wire action={connect signal={example session=123} action={growl text="example"}} %}

The action above will trigger the grow action when a signal of type example and with property ``{session, 123}`` 
is emitted. It is possible to make multiple matches on the property of a signal.

Actions
=======

The module has three actions which can be used to connect and disconnect to signals, and an action to emit signals.

Connect
-------

.. include:: action_connect.rst

Emit
----

Example::

  {emit signal={signal_type prop1=123}}

========== ===========
Attributes Description
========== ===========
signal     The signal you want to emit. When there is a slot which matches the pattern of the emitted 
           signal, it will receive the signal. Note that it is possible that multiple slots match this signal. 
========== ===========

Disconnect
----------

Example::

  {disconnect name="my-signal"}

========== ===========
Attributes Description
========== ===========
name       Disconnect from the named slot. 
========== ===========

Model
=====

m.signal

Makes it possible to access the emitted signal in a template. 

m.signal[signal].type - The type of the emitted signal

