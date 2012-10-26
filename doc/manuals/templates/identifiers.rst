.. _manual-template-autoids:

Auto-generated identifiers
--------------------------

If you include a template like the one above into your dialog template
many times (i.e. from a for loop), then having fixed id's are no
good. Zotonic provides a mechanism to generate an identifer which has
a unique value within the template.

To prefix the id with a unique value (per invocation of the
template) prefix the id with a ``#``-sign::

  <div id="{{ #foo }}">

This special notation will replace ``#foo`` with an auto-generated
identifer, which will expand to something like this::

  <div id="ubifgt-foo">
  

