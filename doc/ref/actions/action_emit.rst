.. include:: meta-emit.rst

Emit a signal.

Example::

  {emit signal={signal_type prop1=123}}

========== ===========
Attributes Description
========== ===========
signal     The signal you want to emit. When there is a slot which matches the pattern of the emitted 
           signal, it will receive the signal. Note that it is possible that multiple slots match this signal. 
========== ===========
