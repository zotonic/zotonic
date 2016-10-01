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
You can do this in admin: Structure > Translation.

On this page you can add and remove languages, enable/disable languages and more.

The selected language can optionally get displayed in the URL of the page - useful if language versions of the page should be accessible as permanent URLs, for instance for search engine indexing.

The language code in the URL is either a 2-letter code (e.g. ``en``), a 2-2 language-territory combination (e.g. ``en-gb``), a 2-4 language-script combination (e.g. ``zh-hant``), or a language-territory-territory combination (e.g. ``zh-hant-hk``).


Translation sources
-------------------

Texts in templates are made localizable with different forms of underscore syntax.

Translate tag::

    {_ Translate me _}

Interpolation trans tag::

    {% trans "Hello {foo} World" foo=_"happy" %}

As part of a tag parameter::

    {% button text=_"Click me" %}

Texts in ``erl`` files use the ``?__()`` syntax::

    ?__("Translate me", Context)

and with binary strings::

    ?__(<<"Translate me">>, Context)

using string concatenation::

    [?__("Edit:", Context), " " ++ Title]

and with binary strings::

    ?__(<<"Edit:"/binary, " ", Title/binary>>, Context)


Generation of translations
--------------------------

The fixed texts in a Zotonic website are translated using the `GNU
gettext <http://www.gnu.org/software/gettext/>`_ ``.po`` file format and
its related tools. If you’re not using our :ref:`guide-docker` images, you may
have to install gettext first:

.. code-block:: shell

    $ sudo apt-get install gettext

In Zotonic, translations files are placed in two locations:

- for the core modules, the translation files are consolidated in
  :file:`priv/translations/`;
- third-party modules and sites, including your own, have their translation
  files in a :file:`translations/` subdirectory in the module itself.

In the translations directory you can find the ``.po`` files containing the
translations. They are marked with the their language code. (Optionally you can name your file like:
nl.foobar.po as Zotonic will only look at the part till the first '.'
for the language code)::

    mod_foo
    └── translations/
    ├── nl.po
    ├── template
    │   └── mod_foo.pot
    └── tr.po
    └── zh-hant.po

This shows that module ``mod_foo`` has been translated into
Dutch (`nl`), Turkish (`tr`) and Chinese traditional script (`zh-hant`).

The ``.po`` translation files are based on translation templates (``.pot``
files). The templates are located in :file:`translations/templates`:

- :file:`priv/translations/template/zotonic.pot` for the core modules;
- :file:`mod_foo/translations/template/mod_foo.pot` for custom modules.

This ``.pot`` file is regenerated when you click on the ‘Generate .pot files’
button on the :ref:`Translation page <mod_translation>` in the admin.
Alternatively,
from your Zotonic shell:

.. code-block:: erlang

    mod_translation:generate(Context).

Zotonic will parse all your templates and Erlang modules for translatable
strings. These strings are then added to the ``.pot`` files.

Creating a new translation for a module
.......................................

First, add a language in the admin with the language code for that language. See the Translation page (or the code in ``src/i18n/languages.erl``) for possible languages.

Say, we're adding Polish, ``pl``. Now copy the ``.pot`` template file
to the language code ``.po`` file::

  $ cd modules/mod_foo
  $ cp translations/template/mod_foo.pot translations/pl.po

Now, open ``pl.po`` in your favorite editor and start translating the strings.
A good po file editor can be found at: http://www.poedit.net/

Updating translation strings
............................

When templates change, often the translatable strings change: more
strings are added, strings are changed, or removed. When this happens,
the translation files need to be kept in sync.

In admin: Structure > Translation > Translation Status you can find the overview of the amount of strings that are translated for each language.

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

To do this for all files, without backup::

    $ find . -name "*.po" -print0 | xargs -0 -I file msguniq file -o file


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
setting the config key ``i18n.language_stemmer`` to the two letter language code of
the language matching with the stemmer. You have to make sure that the stemmer
is configured in PostgreSQL otherwise the pivot process will crash with a SQL error.

