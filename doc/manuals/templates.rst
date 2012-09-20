.. _manual-templates:

Templates
=========

Lorem ipsum...


.. _manual-template-language:

Template language
-----------------

The template engine is based on ErlyDTL, which is a port of the Django template language to Erlang.


.. _manual-lookup-system:

Lookup system
-------------

Lore-di-lore-da...


.. _manual-template-tools:

Template tools
--------------

The building blocks of a template.


.. _manual-scomps:

Scomps
^^^^^^

Screen components for when tags and included templates are not enough and programming is needed.

Scomps add logic to templates or generate HTML/javascript constructs that are too difficult for templates. A good example is the :ref:`scomp-menu` scomp which implements the menu, including sub menus and highlighting of the current menu item.

Scomps have the same syntax as tags::

   {% scompname arg=value ... %}.

Scomps are implemented by :ref:`modules`, tags are implemented by the template system.

.. seealso:: a listing of all :ref:`scomps`.


.. _manual-tags:

Tags
^^^^

Tags add logic and flexibility to your templates.

Zotonic tags are in general compatible with Django tags, though there are some differences in which arguments are accepted. Not all Django tags are implemented.

Zotonic also adds some new tags and `Filters` unknown to Django. For example the tags :ref:`tag-image` and :ref:`tag-trans`.

.. seealso:: listing of all :ref:`tags`.


.. _manual-models:

Models
^^^^^^

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

.. seealso:: listing of all :ref:`models`.


.. _manual-filters:

Filters
^^^^^^^

Filters are used to modify values you want to show or use in your templates.

.. seealso:: a listing of all :ref:`filters`.


.. _manual-actions:

Actions
^^^^^^^

Attach AJAX and jQuery handlers to HTML elements and events.

Actions are the basis of all interactivity on a Zotonic web page.

Actions can be connected to HTML elements or Javascript events using the :ref:`scomp-wire` scomp.

Actions range from a simple jQuery :ref:`action-show` to Ajax :ref:`action-postback`\s that can trigger many other actions. The server also replies to a :term:`postback` or :term:`Comet` push with actions to be executed on the browser.

.. seealso:: listing of all :ref:`actions`.


.. _manual-validators:

Validators
^^^^^^^^^^

Validators for HTML form fields.

Validators check if form fields have an acceptable value. They check both client side and server side if the input fields are valid.

When an input field has been verified then it is available to Erlang programs via the function `z_context:get_q_validated/2`.

When a client side input field does not validate on the server side then the complete form submit is refused.

.. seealso:: listing of all :ref:`validators`, and the :ref:`scomp-validate` scomp.


.. _manual-wiring-events:

Interactivity: wiring events
----------------------------

wire wire pants on fire.

