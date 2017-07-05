.. _cookbook-custom-filter:

Create a custom filter
======================

Create custom :ref:`template filters <guide-filters>` to change the way
variables are rendered in your templates. By following some simple rules,
Zotonic will automatically find the filter for you:

1. Create a file in the :ref:`module-directory-filters` directory of
   your site or module.
2. Prepend the filter filename with ``filter_``.
3. Export a function with the name of your filter that corresponds to the
   filename.

So, let’s say you need to sort a list of items and remove duplicate values from
it:

.. code-block:: erlang
    :caption: mod_yourmodule/filters/filter_uniquesort.erl

    -module(filter_uniquesort).
    -export([uniquesort/2]).
    -include_lib("zotonic_core/include/zotonic.hrl").

    uniquesort(List, _Context) ->
        lists:usort(List).

The custom ``uniquesort`` filter is then available in your templates:

.. code-block:: django

    {% for thing in list_of_things|uniquesort %}
        {{ thing }} is now sorted and unique!
    {% endfor %}

.. seealso::

    * :ref:`guide-templates` guide
    * :ref:`filters` reference
