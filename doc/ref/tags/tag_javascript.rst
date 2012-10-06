.. index:: tag; javascript
.. _tag-javascript:

javascript
==========

Adds javascript that will be run after jQuery has been initialized.
In dynamic content it will run after the DOM has been updated with the
template where the javascript was defined.

Example::

  {% javascript %}
    ...
  {% endjavascript %}