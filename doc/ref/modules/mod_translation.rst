
.. include:: meta-mod_translation.rst

This module provides support for dealing with multiple languages.

How content and static strings are translated is explained in full in
:ref:`manual-i18n`.

             
Language as part of the URL
---------------------------

By default, :ref:`mod_translation` prefixes each URL (using
:ref:`manual-dispatch-rewriting`) in your website with the code of the
current language. The idea behind this is that each language version
of a :term:`resource` gets its own URL, and is as such indexable for
Google.

This behaviour is enabled by default, but can be switched off by
setting the config key ``mod_translation.rewrite_url`` to ``false``.


Programmatically switching languages
------------------------------------

In a template, you can use :ref:`mod_translation`'s `postback` hook
to switch between languages::

  {% button text="Dutch" postback={set_language code="nl"} delegate=`mod_translation` %}

Creates a button which switches to Dutch. And another one for english::
       
  {% button text="English" postback={set_language code="en"} delegate=`mod_translation` %}


Supporting right-to-left languages
----------------------------------

To support right-to-left languages like Arabic and Hebrew, you need to
make some changes to your templates. mod_translation adds some support to help you.

The idea is to give your ``<body/>`` tag the text direction of the main
item visible on the page. Individual items, for example in menus or
context lists, need their own text direction.

You can specify the text direction element attributes by including a template::

  <body {% include "_language_attrs.tpl" id=id %} >

This will generate the following, when Zotonic selected Arabic for the page with id `id`::

  <body xml:lang="ar" lang="ar" dir="rtl" class="rtl">
  
When you want to add an extra class added to the rtl or ltr class you can use::

  <body {% include "_language_attrs.tpl" id=id class="my-body-class" %} >
  
And when you want to force a specific language::

  <body {% include "_language_attrs.tpl" language=`en` %} >
  
