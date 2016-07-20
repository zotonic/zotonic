.. highlight:: django
.. _guide-translation:

Translation
===========

Many sites need to support content and templates in multiple
languages. Luckily, Zotonic is completely multilingual, out of the
box. :ref:`mod_translation` does all the work for you.

Zotonic will try to serve the page in the language of the user (based
on the browser’s `HTTP Accept header
<http://en.wikipedia.org/wiki/Content_negotiation>`_) or the selected
language from the URL. If this fails, Zotonic falls back to the
default language as configured on the Translation admin page.

In Zotonic, two kinds of information are being translated.

- Static translations: The fixed texts in your templates, like button
  labels, confirmation texts, etc.

- Translated content, as edited in the admin. The text fields of each
  :term:`resource` can be translated in multiple languages.


Enabling multiple languages
---------------------------

Before you can use multiple languages, you first need to enable
:ref:`mod_translation`.

Next, you need to tell Zotonic which languages you are going to use.
You can do this in the /admin, following the `Translation` menu item
in the `Structure` submenu.

On this page you can add and remove languages, enable/disable
languages and more. Note that a language is always identified by its
two letter `ISO639-1 language code
<http://nl.wikipedia.org/wiki/Lijst_van_ISO_639-1-codes>`_.


Translation sources
-------------------

Texts in templates are made localizable with different forms of underscore syntax.

Translate tag::

    {_ Translate me _}

Extended translate tag::

    {% _ "Example" nl="Voorbeeld" fr="Exemple" %}

As part of a tag parameter::

    {% button text=_"Click me" %}

Texts in ``erl`` files use the ``?__()`` syntax::

    ?__("Translate me", Context)

and using string concatenation::

    [?__("Edit:", Context), " " ++ Title]


Generation of translations
--------------------------

The fixed texts in a Zotonic website are translated using the `GNU
gettext <http://www.gnu.org/software/gettext/>`_ ``.po`` file format and
its related tools.

In Zotonic, static translations are organized in each Zotonic
module. It is the module’s responsibility to provide translations for
all the texts that it uses in its templates. All files related to
static translations live inside the ``translations/`` subdirectory of
a module (remember: a Zotonic site is just a module!).

In the ``translations/`` directory of the modules you can find the ``.po``
files containing the translations. They are marked with the their two
letter language code.  (Optionally you can name your file like:
nl.foobar.po as Zotonic will only look at the part till the first '.'
for the language code)::

  translations/
  ├── nl.po
  ├── template
  │   └── mod_foo.pot
  └── tr.po

Here you see that this module, ``mod_foo``, has been translated into
Dutch (`nl`) and Turkish (`tr`).

The translations/ directory contains a subdirectory which contains
``mod_foo.pot``, which is the translation `template`, on which new
translations will be based.

The basis for these files (the translation `template`), is the ``.pot``
file which is located in the ``template/`` subdirectory of the translations
directory. This ``.pot`` file is regenerated when you click on the ‘Generate
.pot files’ button on the Translation page in the admin. Alternatively,
from your Zotonic shell:

.. code-block:: erlang

    mod_translation:generate(Context).

Zotonic will parse all your templates and Erlang modules for translatable
strings. These strings are then added to the .pot file.

Creating a new translation for a module
.......................................

First, add a language in the admin with the 2-letter code for that language.

Say, we're adding Polish, ``pl``. Now copy the ``.pot`` template file
to the 2-letter code ``.po`` file::

  $ cd modules/mod_foo
  $ cp translations/template/mod_foo.pot translations/pl.po

Now, open ``pl.po`` in your favorite editor and start translating the strings.
A good po file editor can be found at: http://www.poedit.net/

Updating translation strings
............................

When templates change, often the translatable strings change: more
strings are added, strings are changed, or removed. When this happens,
the translation files need to be kept in sync.

The `Translation status` page in the admin gives an overview, per
module / language combination, of the amount of strings that are
translated for each language.

After pressing the `Generate .pot files` button in the translation
admin the ``.pot`` files are updated, but the existing ``.po`` files
are not.

GNU gettext comes with a tool, ``msgmerge``, which looks at the
changed strings in a ``.pot`` file and changes the translated strings
in a language’s ``.po`` file accordingly::

  $ cd modules/mod_foo/translations
  $ msgmerge -U -N nl.po template/mod_foo.pot

This will merge the new strings into the existing ``nl.po``
file.

To update all ``.po`` files in the directory::

  $ cd modules/mod_foo/translations
  $ find . -name "*.po" -print0 | xargs -0 -I file msgmerge -U -N file template/*.pot

After doing this, you'll need to edit each ``po`` file again
to check if there are new strings which need translation, edit
existing strings, etc.

Helpful commands
````````````````

To remove duplicates (and make a backup first), use::

    $ cat nl.po > nl~.po && msguniq nl.po -o nl.po



Translated content
------------------

When you have enabled languages on the Translation page of the admin
you will see a Translations item on the right of the edit page.

Each language has a checkbox next to it: When you click a checkbox,
the language will become visible as a tab on your content items.

Resources in Zotonic are translated on a per-page basis. This allows
you to start translating your site by translating the most important
pages first.


Text searches, translations and stemming
----------------------------------------

For text searches a full text index is maintained. This full text index
is stemmed according to the site’s configured default language.

All translations are added to the same full text index. This combined text is
stemmed using a single stemmer. The selected stemmer depends on the configured
default language (config key ``i18n.language``). The stemmer can be overruled by
setting the config key ``i18n.language_stemmer`` to the two letter iso code of
the language matching with the stemmer. You have to make sure that the stemmer
is configured in PostgreSQL otherwise the pivot process will crash with a SQL error.

