.. include:: meta-mod_signal.rst

`mod_signal` allows template developers to create pages with highly interactive behaviour. It allows one to 
emit so called signals. Signals are asynchronous events with attributes. Other pages can connect to the signature 
of a signal. When that signal is fired, the specified action is triggered. This makes it possible to create 
interactive pages without writing a line of erlang code.

Signals
-------

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
-------

The module has three actions which can be used to connect and disconnect to signals, and an action to emit signals.

``connect``
  Connect actions to signals. The actions are executed when the signal is emitted. For more information 
  see: :ref:`action-connect`.
``emit``
  Emit a signal from a template. See: :ref:`action-emit`.
``disconnect``
  Disconnect all actions. The actions will no longer be executed when the signal is emitted. 
  See: :ref:`action-disconnect`.

Model
-----

Model ``m.signal`` makes it possible to use the data of the emitted signal inside a template. See: :ref:`model-signal`.

.. seealso:: :ref:`action-connect` :ref:`action-emit` :ref:`action-disconnect` :ref:`model-signal`
