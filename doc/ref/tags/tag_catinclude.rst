.. index:: tag; catinclude
.. _tag-catinclude:

catinclude
==========

Include another template based on the category of a resource. The include tag is replaced with the contents of the included template file. You can give arguments to the included template, they will be assigned as variables in the context of the included template.

Example::

   {% catinclude "hello.tpl" id %}

Assuming that the resource whose id is the value of the template variable `id` is a news article then catinclude will consider the following templates::

   hello.news.tpl
   hello.article.tpl
   hello.text.tpl
   hello.tpl

This because `news` is a subcategory of `article`, which is a subcategory of `text`. When one of the previous templates is not found then the base template `hello.tpl` is tried. The catinclude tag will only include the first file it finds, and stops after having found a file to include.

Unlike Django the template name must be a string literal, variables are not allowed.

The tag accepts extra arguments, which will be passed as template variables to the included template. The inclusion is always done at runtime, because the selected template depends on the category of the referenced resource.

The resource id will be available in the included template as the variable `id`.

See the :ref:`tag-include` for caching options and argument handling.

.. seealso:: :ref:`tag-all-catinclude`, which is useful to include multiple templates.
