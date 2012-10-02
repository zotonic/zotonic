.. highlight:: django
.. include:: meta-group_firstchar.rst

Group a list of `sorted` :ref:`resource <model-rsc>` ids on their
first letter of the title or another rsc property. After grouping, it
splits this list in a number of more-or-less even columns.

This is useful for displaying multiple columns of alphabetically
sorted pages, in which the pages are grouped by the first letter, for
instance a search result.

For instance, this piece of template code::

  <table>
  {% for cols in m.search[{query cat="country" sort="pivot_title"}]|group_firstchar:"title":3 %}
     <td>
        {% for group in cols %}
        <b>{{ group.first }}</b>
        {% for id in group.result %}
        <li>
            {{ id.title }}
        </li>
        {% endfor %}
     {% endfor %}
     </td>
  {% endfor %}
  </table>

Groups the list of ids by title in three columns. It then uses nested
for-loops to render a table similar to this::

  +----------------+----------------+----------------+
  |A               |D               |G               |
  | Afghanistan    | Denmark        | Georgia        |
  | Azerbeidjan    |                | Germany        |
  |                |E               | Grenada        |
  |B               | Ecuador        | Guadeloupe     |
  | Belgium        | Egypt          | Greenland      |
  | Belarus        | Eritrea        | Guinea         |
  | Brazil         | Estonia        |                |
  |                |                |H               |
  |C               |F               | Haiti          |
  | Canada         | Fiji           |                |
  |                | Finland        |                |
  +----------------+----------------+----------------+

As you can see, all three columns have approximately the same size,
although the size of the indiviual groups differs.

When no nr. of columns is given, the groups are returned in a single
column.

.. seealso:: :ref:`filter-group_title_firstchar`
