.. highlight:: django
.. include:: meta-remove.rst

Remove an element from the page.

For example, the following removes the `foo` div from the page::

  <div id="foo">I am the foo div</div>
  {% button text="Remove foo" action={remove target="foo"} %}

Without target, the action removes its triggering element::

  {% button text="Click me to remove me" action={remove} %}

.. seealso:: :ref:`actions`, :ref:`scomp-button`
