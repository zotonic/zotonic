.. highlight:: django
.. include:: meta-filter_not.rst

Filters a list on the value of a property, either on absence or inequality.

This is the inverse of :ref:`filter-filter`.

Testing presence
----------------

To filter a list of values::

  {% print somelist|filter_not:`p` %}

Results in a list where all elements **do not have** the property ``p`` defined and
where the property (after conversion to boolean) is ``false``.

This can be used to filter a list of resource ids on the absence of a property. For example, to see all unpublished elements in a list of resource ids::

  {% print [1,2,3,4,5,6]|filter_not:`is_published` %}

To find all pages from page connection ``hasdocument`` that **do not have** an image::

  {% print id.o.hasdocument|filter_not:`depiction` %}


Testing equality
----------------

A second argument can be added to test on inequality::

  {% print somelist|filter_not:`title`:"Untitled" %}

Shows all elements whose ``title`` property **is not** "Untitled".

Below is another example of inversely filtering a list::

  {% with m.search[{latest cat='gallery'}] as result %}
    {% if result.total > 0 %}
      {%  with result|filter_not:`name`:"page_home_gallery"|random as gallery_rsc_id %}
        {% include "_gallery_widget.tpl" id=gallery_rsc_id %}
      {% endwith %}
    {% endif %}
  {% endwith %}

The example above filters against a search result and returns only
elements whose ``name`` **is not** "page_home_gallery".



.. seealso:: :ref:`filter-is_visible`, :ref:`filter-is_a`, :ref:`filter-filter`
