.. highlight:: django
.. _guide-templates:

Templates
=========

Templates are text files marked up using the Zotonic template language. Zotonic
interprets that mark-up to dynamically generate HTML pages. Zotonic’s template
syntax is very similar to the Django Template Language (DTL).

.. _template-variables:

Variables
---------

Variables are surrounded by ``{{`` and ``}}`` (double braces)::

    Hello, I’m {{ first_name }} {{ last_name }}.

When rendering this template, you need to pass the variables to it. If you pass
``James`` for ``first_name`` and ``Bond`` for ``last_name``, the template
renders to::

    Hello, I’m James Bond.

Instead of strings, variables can also be objects that contain attributes. To
access the attributes, use dot notation::

    {{ article.title }} was created by {{ article.author.last_name }}

The variables that you add to your templates get their values from thee places:

* they can be :ref:`passed from controllers <guide-render>` when rendering the
  template;
* they can come from :ref:`models <guide-models>`;
* or they can be :ref:`global variables <ref-global-variables>`.

To print the variables in your template for debugging, you can use the
:ref:`scomp-debug` tag::

    {% debug %}

.. _guide-filters:

Filters
-------

Optionally, you can pipe template variables through filters. Filters transform
the value before rendering it. To apply a filter, you append ``|`` plus the
filter’s name to the variable::

    {{ first_name|lower }} {{ last_name|upper }}

Renders::

    james BOND

There’s a :ref:`reference of built-in filters <filters>`. If you need to, you
can also :ref:`create your own filters <cookbook-custom-filter>`.

.. _guide-tags:

Tags
----

With tags, you can add logic and flexibility to your templates. Tags are
surrounded by ``{%`` and ``%}``, like this::

    {% tag %}

Some tags have arguments::

    {% tagname id width=200 height=200 crop %}

And some tags are *block tags*, which require both an opening and a closing tag.
The name of the closing tag is always ``end`` plus the name of the opening tag::

    {% if article.is_published %}
        There you go:: {{ article.title }}
    {% else %}
        Sorry, the article hasn’t been published yet!
    {% endif %}

See also the reference :ref:`all tags <tags>`. You can also
:ref:`create your own tags <cookbook-custom-tag>`.

.. _guide-models:

Models
------

A template model provides data to templates through the syntax:
``m.modelname.property``. For example::

    {# Get the site's title #}
    {{ m.site.title }}

    {# Fetch the title of the page with name page_home #}
    {{ m.rsc.page_home.title }}

    {# Fetch the title of the page whose id is the integer 1 #}
    {{ m.rsc[1].title }}

    {# Perform a search on all persons #}
    {% for p in m.search[{query cat='person'}] %}{{ p.title }}{% endfor %}

    {# Fetch the title of the page whose id is the template variable id #}
    {{ m.rsc[id].title }}

You’ll find that you need ``m.rsc[id]`` the most, so there’s a
:ref:`recommended shortcut <best-practices-shortcut-syntax>` for that::

    {{ id.title }}

See the reference for a list of :ref:`all models <models>`. You can also add
:ref:`your own models <cookbook-custom-model>`.

.. _guide-lookup-system:

Template names
--------------

All templates are stored in the :file:`priv/templates/` directory of
:ref:`sites <sites>` and :ref:`modules <guide-modules>`. They have the
extension ``.tpl``. Templates are referred to by their filename, including their
subdirectory name within :file:`priv/templates/` (if any). So if you have these two
templates:

* ``modules/mod_example/priv/templates/foobar.tpl``
* ``modules/mod_example/priv/templates/email/email_base.tpl``

you refer to them as:

* ``foobar.tpl``
* ``email/email_base.tpl``

The module name itself (``mod_example``) is never part of the template name,
because all templates are grouped together. This allows you to override
Zotonic’s templates.

.. _overriding-templates:

Overriding templates
^^^^^^^^^^^^^^^^^^^^

If you want to override a template, you create a template with the same name
in your site (or module). So what if the ``email/email_base.tpl`` template from
mod_example mentioned above is not to your liking? Just create a
``email/email_base.tpl`` file in your own site:
:file:`sites/yoursite/priv/templates/email/email_base.tpl`.

So if multiple templates can have the same name, how does Zotonic know *which*
template to render: the one from mod_example or the one from yoursite? This depends
on the :ref:`module priority <module-priority>`. Usually sites have a higher
priority than modules, so yoursite wins and can serve its template.

If you want to *add* your template instead of overriding, you can use the
:ref:`tag-all-include` and :ref:`tag-all-catinclude` tags.

Page templates
^^^^^^^^^^^^^^

Most of your templates will :ref:`page <pages>` templates. When showing
a page, Zotonic’s :ref:`page controller <controller-page>` looks up the
appropriate template in order of specificity and renders the first template it
finds:

1. ``page.name.tpl`` (:ref:`unique name <model-rsc>`)
2. ``page.category.tpl`` (:ref:`category <categories>`)
3. ``page.tpl`` (fallback)

So if you have a page in the category ‘text’ and that page has a unique name
‘my_text_page’, Zotonic will look for the following templates:

1. ``page.my_text_page.tpl`` (unique name)
2. ``page.text.tpl`` (category)
3. ``page.tpl`` (fallback)

The same lookup mechanism is used for the :ref:`tag-catinclude` tag.

Template structure
------------------

Now you know how Zotonic decides which template to render for a page, let’s go
into how you can render templates yourself. Usually, you spread template logic
for a page over multiple template files. This allows you to re-use these files
in other templates.

Including templates
^^^^^^^^^^^^^^^^^^^

You can include other templates using the :ref:`tag-include` tag::

    This is the main template. To include another template:

    {% include "other-template.tpl" %}

Zotonic will replace the include tag with the output of
:file:`other-template.tpl`.

Variants of the include tag are
:ref:`tag-catinclude`, :ref:`tag-all-include` and :ref:`tag-all-catinclude`:
following the :ref:`lookup mechanism <guide-lookup-system>` described above,
Zotonic will find the best template based on the page and module priority.

Inheritance
^^^^^^^^^^^

To improve template re-use, it is common to inherit from a base template. A
simple base template might look like this:

.. code-block:: django
    :caption: templates/base.tpl

    <!DOCTYPE html>
    <html>
        <head>
            <title>Zotonic{% block title %}{% endblock %}</title>
        </head>

        <body>
            {% block content %}This is default content{% endblock %}
        </body>
    </html>

You can then extend multiple templates from this single base template using the
:ref:`tag-extends` tag. The base template contains :ref:`tag-block` tags that
can be overridden in child templates:

.. code-block:: django
    :caption: templates/page.tpl

    {% extends "base.tpl %}

    {% block title %}This is the page title{% endblock %}

    {% block content %}
        This will override the content block from base.tpl
    {% endblock %}

Using the :ref:`tag-inherit` and :ref:`tag-overrules` tags, you adapt the
inheritance to your needs.

.. seealso::

    - :ref:`guide-media` on how to include images and other media in your
      templates.
    - :ref:`guide-wires` on using JavaScript to add interaction to your
      templates.
