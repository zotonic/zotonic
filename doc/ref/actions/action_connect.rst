.. include:: meta-connect.rst

The connect action allows one to attach other actions to a specified signal. When the signal is emitted, the 
specified actions are performed.

Example::

  {connect signal={signal_type prop1=value} action={...}}

========== ===========
Attributes Description
========== ===========
signal     The pattern of a signal to which you want to connect to.
action     The action you want to perform when a signal matching the pattern is emitted.
name       The name of the connection. Giving a connection a name makes it possible to disconnect it at a later stage.
========== ===========