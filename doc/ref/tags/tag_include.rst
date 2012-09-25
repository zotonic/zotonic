
.. index:: tag; include
.. _tag-include:

include
=======

Include another template. The include tag is replaced with the contents of the included template file. You can give arguments to the included template, they will be assigned as variables in the context of the included template.

Example::

   {% include "_hello.tpl" name="Peter" %} world.

When `_hello.tpl` contains the text::

   Hello {{ name }}'s

Then this will output the text ``Hello Peter's world.``.

Unlike Django the template name must be a string literal, variables are not allowed.

.. note::
   About unique ids
      :index:`Automatically generated ids` (``{{ #name }}``) are :index:`unique <pair: unique; id>` within an included template and do not clash with similarly named ids in the including template.

.. seealso:: :ref:`tag-all-include` and :ref:`tag-catinclude`.


Caching of the included template
--------------------------------

The output of the included template can be cached. This is useful when rendering the template takes considerable time. For example when the template shows a list of recent news items, which comprises a query, fetching and rendering a list of news items.

Caching is enabled by defining one of the caching arguments:

+------------+--------------------------------------------------------+--------------------+
|Argument    |Description                                             |Example             |
+============+========================================================+====================+
|maxage      |The maximum time the output can be cached, in seconds.  |maxage=3600         |
|            |Specifying 0 for the maximum age does not cache the     |                    |
|            |output but does protect agains slam dunks, multiple     |                    |
|            |requests rendering the same template at the same time   |                    |
|            |will share the output of the rendering.                 |                    |
|            |                                                        |                    |
+------------+--------------------------------------------------------+--------------------+
|vary        |Dependency keys for the cached output. When a cache key |vary="news"         |
|            |with the same name is flushed or invalidated then the   |                    |
|            |cached output of this template is also invalidated. You |                    |
|            |can use category names here.                            |                    |
|            |                                                        |                    |
+------------+--------------------------------------------------------+--------------------+
|visible_for |Sets the access control user for rendering the included |visible_for="world" |
|            |template.  With this you can for example force to only  |                    |
|            |show public news for logged on users.  Valid values are |                    |
|            |"user", 3, "group", 2, "community", 1, "world",         |                    |
|            |"public", 0                                             |                    |
|            |                                                        |                    |
+------------+--------------------------------------------------------+--------------------+
|sudo        |When supplied then access control is disabled whilst    |sudo                |
|            |rendering the included template. This will show any     |                    |
|            |content not visible for the current user.  Use with     |                    |
|            |care.                                                   |                    |
|            |                                                        |                    |
+------------+--------------------------------------------------------+--------------------+

.. versionadded:: 0.6
   Added the `sudo` option.
