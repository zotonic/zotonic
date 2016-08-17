
.. include:: meta-mod_translation.rst

This module provides support for dealing with multiple languages.

How content and static strings are translated is explained in full in
:ref:`guide-translation`.


Language as part of the URL
---------------------------

By default, :ref:`mod_translation` prefixes each URL (using
:ref:`guide-dispatch-rewriting`) in your website with the code of the
current language. The idea behind this is that each language version
of a :term:`resource` gets its own URL, and is as such indexable for
Google.

This behaviour is enabled by default, but can be switched off in the
admin, by going to `Structure`, `Translation`. There is a checkbox labelled "Show the language in the URL".

Alternatively you can set the config key
``mod_translation.rewrite_url`` to ``false``.


Programmatically switching languages
------------------------------------

In a template, you can use :ref:`mod_translation`’s `postback` hook
to switch between languages::

  {% button text="Dutch" postback={set_language code="nl"} delegate=`mod_translation` %}

Creates a button which switches to Dutch. And another one for english::

  {% button text="English" postback={set_language code="en"} delegate=`mod_translation` %}


Supporting right-to-left languages
----------------------------------

For basic use you don't need to do anything. Zotonic base site adds a ``lang`` attribute to the html tag, and when a right-to-left language is selected (for instance Arabic), the browser will interpret ``lang="ar"`` and automatically adapt the content to right-to-left.

Custom right-to-left content
............................

If you write your own templates, you can add the ``lang`` tag in the html or body tag, for instance::

  <body {% include "_language_attrs.tpl" id=id %} >

This will generate the following, when Zotonic selected Arabic for the page with id `id`::

  <body xml:lang="ar" lang="ar" dir="rtl" class="rtl">

When you want to add an extra class added to the rtl or ltr class you can use::

  <body {% include "_language_attrs.tpl" id=id class="my-body-class" %} >


To create individual right-to-left elements, you can use the same principle::

  <div {% include "_language_attrs.tpl" %}></div>

And when you want to force a specific language::

  <div {% include "_language_attrs.tpl" language=`en` %} >This is English content</div>

