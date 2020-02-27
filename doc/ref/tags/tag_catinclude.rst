
.. index:: tag; catinclude
.. _tag-catinclude:

catinclude
==========

Include another template based on the category of a resource. The include tag is replaced with the contents of the included template file. You can give arguments to the included template, they will be assigned as variables in the context of the included template.

Example::

   {% catinclude "hello.tpl" id %}

Assuming that the resource whose id is the value of the template variable `id`, and that the unique name property of the resource
is ``my_page_name`` is a news article then catinclude will consider the following templates::

   hello.name.my_page_name.tpl
   hello.news.tpl
   hello.article.tpl
   hello.text.tpl
   hello.tpl

This because `news` is a subcategory of `article`, which is a subcategory of `text`. When one of the previous templates is not found then the base template `hello.tpl` is tried. The catinclude tag will only include the first file it finds, and stops after having found a file to include.

If the resource has a unique name (the `name` property is set), this property is also considered for the catinclude lookup, before the category-based template names. If the resource has `name` set to `foobar`, it will first look for ``hello.name.foobar.tpl``, then for ``hello.news.tpl``, etc.

The tag accepts extra arguments, which will be passed as template variables to the included template. The inclusion is always done at runtime, because the selected template depends on the category of the referenced resource.

The resource id will be available in the included template as the variable ``id``.

Instead of passing an id, you can also pass in a list of category names which are to be search for. These
names need to be atoms or strings, for example::

   {% catinclude "hello.tpl" [`text`, `article`] %}

This will search for the following templates, in order::

   hello.article.tpl
   hello.text.tpl
   hello.tpl

**Note** the search order is reversed from the list order, you should add the *most specific selector last*!

See the :ref:`tag-include` for caching options and argument handling.

.. seealso:: :ref:`tag-all-catinclude`, which is useful to include multiple templates.
