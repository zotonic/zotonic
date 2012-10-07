
.. index:: tag;  print

.. _tag-print:

print
=====

Show the contents of a value expression.

The ``{% print %}`` tag is used to dump the contents of a variable in
a HTML safe way.  It is very useful for debugging and inspecting
variables during template development.

For example::

  {% print value %}

When value is “<b>Hello</b> world!” then the example above returns the output::

  <pre>
  &lt;b&gt;Hello&lt;/b&gt; world!
  </pre>

It can also print complex values like nested lists and tuples, for
which it uses Erlang's ``io_lib:format/2`` function.

.. seealso:: :ref:`scomp-debug`
