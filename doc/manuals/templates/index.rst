.. highlight:: django
.. _manual-templates:

Templates
=========

The templates in Zotonic are based on the Django Template Language
(DTL), using a customized version of the excellent `ErlyDTL
<https://github.com/evanmiller/erlydtl>`_ library. Over the years,
Zotonic's version of ErlyDTL has diverged, adding Zotonic-specific
features and more powerful expression possibilities. However, the main
template syntax remains the same:

The double accolade constructs outputs the value of the variable on the screen::

  {{ foo }}

Optionally, you can `pipe` these variables through a so-called filter,
which is applied before output::

  {{ foo|lower }}

Template `tags` allow you to express complex contructs like loops and branches::

  {% if username == 'Arjan' %} Hi, Arjan {% endif %}
  

Being a web framework, it is of no surprise that templates are one of
the most broad topics in Zotonic. The template manual has therefore
been split up into the following sections, each of which addresses a
specific templates-related topic.

.. toctree::
   :glob:
   :maxdepth: 1

   lookups
   tags
   filter
   actions
   validators
   identifiers
   models
   calling-zotonic
   bestpractices



