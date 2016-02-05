
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
      {children,[]},
      {path,"e"},
      {left,1000000},
      {right,1000000}],
     [{id,104},
      {parent_id,undefined},
      {level,1},
      {children,[[{id,106},
                  {parent_id,104},
                  {level,2},
                  {children,[[{id,109},
                              {parent_id,106},
                              {level,3},
                              {children,[]},
                              {path,"hjm"},
                              {left,4000000},
                              {right,4000000}]]},
                  {path,"hj"},
                  {left,3000000},
                  {right,4000000}]]},
      {path,"h"},
      {left,2000000},
      {right,4000000}],
      ...]
 
About the complete category tree
--------------------------------

The following m_category model properties are available in templates:

+--------------+-------------------------------+---------------------------+
|Property      |Description                    |Example                    |
|              |                               |value                      |
+==============+===============================+===========================+
|tree          |Return the complete forest of  |See above.                 |
|              |category trees as nested       |                           |
|              |property lists.                |                           |
+--------------+-------------------------------+---------------------------+
|tree2         |Return the forest of           |See above.                 |
|              |category trees as nested       |                           |
|              |property lists. Up to the      |                           |
|              |children of the children.      |                           |
+--------------+-------------------------------+---------------------------+
|tree_flat     |Return a list of tuples for the|See above entries.         |
|              |category tree. This list is    |                           |
|              |intended for select            |                           |
|              |lists. There is a special field|                           |
|              |for the indentation. The       |                           |
|              |returned list consists of      |                           |
|              |proplists. The                 |                           |
|              |list does not contain the      |                           |
|              |“meta” category, which contains|                           |
|              |the categories “predicate”,    |                           |
|              |“category” etc.                |                           |
+--------------+-------------------------------+---------------------------+
|tree_flat_meta|Same as `tree_flat` but with   |See above entries.         |
|              |the categories in the `meta`   |                           |
|              |category.                      |                           |
+--------------+-------------------------------+---------------------------+


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
|tree1     |The list of direct children     |See above.                           |
|          |below the indexing category.    |                                     |
+----------+--------------------------------+-------------------------------------+
|tree2     |The category tree below and     |See above.                           |
|          |including the indexing category,|                                     |
|          |up to the children of the       |                                     |
|          |children.                       |                                     |
+----------+--------------------------------+-------------------------------------+
|tree_flat |The category tree below and     |See above.                           |
|          |including the indexing category,|                                     |
|          |up to the children of the       |                                     |
|          |children. As a flattened list.  |                                     |
+----------+--------------------------------+-------------------------------------+
|path      |List of parent category ids from|[ 104, 106 ]                         |
|          |the root till the category,     |                                     |
|          |excluding the indexing category.|                                     |
+----------+--------------------------------+-------------------------------------+
|is_a      |List of the parent category     |[ text, article, news ]              |
|          |names form the root till the    |                                     |
|          |category, including the current |                                     |
|          |category.                       |                                     |
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
|level     |The depth of the category. Level|1                                    |
|          |1 is the root, 2 and more are   |                                     |
|          |below the root.                 |                                     |
+----------+--------------------------------+-------------------------------------+
|left      |The lowest value of the nr range|2                                    |
|          |of this category, including its |                                     |
|          |sub categories.                 |                                     |
+----------+--------------------------------+-------------------------------------+
|right     |The highest value of the nr     |8                                    |
|          |range of this category,         |                                     |
|          |including its sub categories.   |                                     |
+----------+--------------------------------+-------------------------------------+
|name      |The unique page name of this    |<<"text">>                           |
|          |category. A binary.             |                                     |
+----------+--------------------------------+-------------------------------------+
|path      |The path through the hierarchy  |[104, 106]                           |
|          |of categories to this category. |                                     |
+----------+--------------------------------+-------------------------------------+
