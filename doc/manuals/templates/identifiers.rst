.. _manual-template-autoids:

Auto-generated identifiers
--------------------------

If you include a template many times (i.e. from a for loop), then having 
fixed element identifiers are no good. Zotonic provides a mechanism to generate 
an identifer which has a unique value within the template.

To prefix the id with a unique value (per invocation of the
template) prefix the id with a ``#``-sign::

  <div id="{{ #foo }}">

This special notation will replace ``#foo`` with an auto-generated
identifer, which will expand to something like this::

  <div id="ubifgt-foo">

Unique ids can also be generated inside a ``for`` loop::

  {% for id in mylist %}
    <li id="{{ #foo.id }}">{{ id.title }}</li>
  {% endfor %}

This will generate html like this::

  <li id="gdjqa-foo-1234">Some great news</li>
