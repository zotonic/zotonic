
.. include:: meta-category.rst

This model can retrieve information about the resource category
hierarchy in different ways.

Categories are the principal categorization (typing) system of
resources. Every page is assigned to exactly one category. Categories
themselves are organized in a tree-like hierarchy.

A category is a resource with a special ``category`` record attached
to it to store metadata related to the category hierarchy. The
``m_category`` model provides accessors to this category tree and
individual category information.

An example of a category tree, as returned by ``{% print m.category.tree %}``::

  [[{id,101},
    {parent_id,undefined},
    {level,1},
    {children,{ok,[]}},
    {name,<<"other">>},
    {path,{ok,[]}}],
   [{id,104},
    {parent,undefined},
    {level,1},
    {children,{ok,[[{id,106},
                    {parent_id,104},
                    {level,2},
                    {children,{ok,[[{id,109},
                                    {parent_id,106},
                                    {level,3},
                                    {children,{ok,[]}},
                                    {name,<<"news">>},
                                    {path,{ok,...}}]]}},
                    {name,<<"article">>},
                    {path,{ok,"h"}}],
                   [{id,105},
                    {parent_id,104},
                    {level,2},
                    {children,{ok,[]}},
                    {name,<<"review">>},
                    {path,{ok,"h"}}],
                   [{id,503},
                    {parent_id,104},
                    {level,2},
                    {children,{ok,[[{id,504},
                                    {parent_id,503},
                                    {level,3},
                                    {children,{ok,...}},
                                    {name,<<...>>},
                                    {path,...}]]}},
                    {name,<<"documentation">>},
                    {path,{ok,"h"}}]]}},
    {name,<<"text">>},
    {path,{ok,[]}}], … 
  ]

About the complete category tree
--------------------------------

The following m_category model properties are available in templates:

+-------------+-------------------------------+---------------------------+
|Property     |Description                    |Example                    |
|             |                               |value                      |
+=============+===============================+===========================+
|tree         |Return the complete forest of  |See above.                 |
|             |category trees as nested       |                           |
|             |property lists.                |                           |
+-------------+-------------------------------+---------------------------+
|tree1        |Return the root element of all |                           |
|             |category trees.                |                           |
+-------------+-------------------------------+---------------------------+
|tree2        |Return the root set and their  |                           |
|             |direct children as category    |                           |
|             |trees.                         |                           |
|             |                               |                           |
+-------------+-------------------------------+---------------------------+
|all_flat     |Return a list of tuples for the|``[…, {106, 2,             |
|             |category tree. This list is    |"&nbsp;&nbsp;&nbsp;&nbsp;",|
|             |intended for select            |<<"article">>}, … ]``      |
|             |lists. There is a special field|                           |
|             |for the indentation. The       |                           |
|             |returned list consists of      |                           |
|             |tuples {CategoryId, Level,     |                           |
|             |NbspLevel, CategoryName} The   |                           |
|             |list does not contain the      |                           |
|             |“meta” category, which contains|                           |
|             |the categories “predicate”,    |                           |
|             |“category” etc.                |                           |
+-------------+-------------------------------+---------------------------+
|all_flat_meta|Same as all_flat but now       |                           |
|             |including the meta category.   |                           |
+-------------+-------------------------------+---------------------------+


About a single category
-----------------------

The m_category has some special properties defined when fetching a
category, they are accessed by id or category name. For example::

  {{ m.category[104].tree }}
  {{ m.category.text.tree }}

+----------+--------------------------------+-------------------------------------+
|Property  |Description                     |Example value                        |
+==========+================================+=====================================+
|tree      |The category tree below and     |See above.                           |
|          |including the indexing category.|                                     |
+----------+--------------------------------+-------------------------------------+
|tree1     |The list of direct children     |[ [{id,106},                         |
|          |below the indexing category.    |{parent_id,104},                     |
|          |                                |{seq,3}, {nr,3},                     |
|          |                                |{lvl,2}, {lft,3},                    |
|          |                                |{rght,4},                            |
|          |                                |{name,<<"article">>},                |
|          |                                |{path,{ok,"h"}}], … ]                |
+----------+--------------------------------+-------------------------------------+
|tree2     |The category tree below and     |See above.                           |
|          |including the indexing category,|                                     |
|          |up to the children of the       |                                     |
|          |children.                       |                                     |
+----------+--------------------------------+-------------------------------------+
|path      |List of parent categories from  |[ 104, 106 ]                         |
|          |the root till the category,     |                                     |
|          |excluding the indexing category.|                                     |
+----------+--------------------------------+-------------------------------------+
|image     |A random depiction for this     |<<"2009/10/20/flat-world-proof.jpg">>|
|          |category. The returned image    |                                     |
|          |filename comes from one of the  |                                     |
|          |pages within this category.     |                                     |
+----------+--------------------------------+-------------------------------------+
|parent_id |The page id of the parent       |104                                  |
|          |category. Returns an integer or,|                                     |
|          |for a root category, undefined. |                                     |
+----------+--------------------------------+-------------------------------------+
|nr        |The category nr. Used for       |2                                    |
|          |building the tree, will change  |                                     |
|          |when categories are added or    |                                     |
|          |removed. An integer.            |                                     |
+----------+--------------------------------+-------------------------------------+
|lvl       |The depth of the category. Level|1                                    |
|          |1 is the root, 2 and more are   |                                     |
|          |below the root.                 |                                     |
+----------+--------------------------------+-------------------------------------+
|lft       |The lowest value of the nr range|2                                    |
|          |of this category, including its |                                     |
|          |sub categories.                 |                                     |
+----------+--------------------------------+-------------------------------------+
|rght      |The highest value of the nr     |8                                    |
|          |range of this category,         |                                     |
|          |including its sub categories.   |                                     |
+----------+--------------------------------+-------------------------------------+
|name      |The unique page name of this    |<<"text">>                           |
|          |category. A binary.             |                                     |
+----------+--------------------------------+-------------------------------------+
|path      |The path through the hierarchy  |{ok, [104, 106]}                     |
|          |of categories to this category. |                                     |
+----------+--------------------------------+-------------------------------------+
