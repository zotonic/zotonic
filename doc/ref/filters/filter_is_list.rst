.. highlight:: django
.. include:: meta-is_list.rst

Test if a value is a list::

  {% if [1,2,3]|is_list %}Yes, this is a list {% endif %}
