.. highlight:: django
.. include:: meta-as_atom.rst

Convert a value to an Erlang atom. 

Atoms are represented in the template language as strings between
backticks, ```like this```. Some template function require their
arguments to be atoms, and this filter helps in converting any value
to such an atom::

  {{ "foobar"|as_atom }}

evaluates to the atom ``foobar``.
