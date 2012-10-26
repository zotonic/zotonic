.. _manual-models:

Template models
===============

Models for storing and accessing data.

Zotonic provides some models to access the different entities in the system.

Here you can read about the models and how to access model properties or methods from templates. Check the source of the :term:`controller` :ref:`modules <controllers>` to learn how to access them from Erlang.

The Erlang modules implementing models always start with `m_` (like in `m_rsc`). In templates they can be accessed using the `m` accessor, for example::

   {# Fetch the title of the page with name page_home #}
   {{ m.rsc.page_home.title}}
   
   {# Fetch the title of the page whose id is the integer 1 #}
   {{ m.rsc[1].title }}
   
   {# Fetch the title of the page whose id is the template variable id #}
   {{ m.rsc[id].title }}

.. note::

   The most important model is :ref:`model-rsc` that implements access to the pages.
   So there is syntactic sugar for accessing the rsc model when you have a template variable holding the page id to access::

      {# Fetch the title of the page whose id is the template variable id #}
      {{ id.title }}


Implementing a model
--------------------

.. todo:: Write how to implement a model and how the `m_find...` functions work

.. seealso:: listing of all :ref:`models`.
