.. highlight:: django
.. include:: meta-group_title_firstchar.rst

Similar to :ref:`filter-group_firstchar`, but always uses the
``title`` column from the rsc table. 

This is merely a shortcut, simplifying the template syntax::

  <table>
  {% for cols in m.search[...]|group_title_firstchar:4 %}
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

Groups alphabetically on title, in four columns.

.. seealso:: :ref:`filter-group_firstchar`
