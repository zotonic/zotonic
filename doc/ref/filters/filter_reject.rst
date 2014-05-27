.. highlight:: django
.. include:: meta-reject.rst

Filters a list on the value of a property, either on absence or inequality.

This is the inverse of :ref:`filter-filter`.

Testing presence
----------------

To filter a list of values::

  {% print somelist|reject:`p` %}

Results in a list where all elements **do not have** the property ``p`` defined and
where the property (after conversion to boolean) is ``false``.

This can be used to filter a list of resource ids on the absence of a property. For example, to see all unpublished elements in a list of resource ids::

  {% print [1,2,3,4,5,6]|reject:`is_published` %}

To find all pages from page connection ``hasdocument`` that **do not have** an image::

  {% print id.o.hasdocument|reject:`depiction` %}


Testing equality
----------------

A second argument can be added to test on inequality::

  {% print somelist|reject:`title`:"Untitled" %}

Shows all elements whose ``title`` property **is not** "Untitled".

Below is another example of inversely filtering a list::

  {% with m.search[{latest cat='gallery'}] as result %}
    {% if result.total > 0 %}
      {%  with result|reject:`name`:"page_home_gallery"|random as gallery_rsc_id %}
        {% include "_gallery_widget.tpl" id=gallery_rsc_id %}
      {% endwith %}
    {% endif %}
  {% endwith %}

The example above filters against a search result and returns only
elements whose ``name`` **is not** "page_home_gallery".



.. seealso:: :ref:`filter-is_visible`, :ref:`filter-is_a`, :ref:`filter-filter`
