.. _cookbook-pivot-templates:

Pivot Templates
===============

:ref:`guide-datamodel-query-model` uses database indices on special pivot
columns and full text fields.

These columns and text fields are extracted in a process called *pivoting*.
This is done a short period after a resource has been updated.

The content of the pivot columns is determined by a template. This template is
called ``pivot/pivot.tpl`` (in ``mod_base``) and is rendered using a *catinclude*.
This makes it possible to have your own unique indexing per category.

The ``pivot/pivot.tpl`` template consists of multiple blocks. Each block corresponds
to a pivot column. The block is rendered for determining the pivot’s content.

The following blocks are defined:

**a, b, c and d**

  These blocks are used for the full text index. Block *a* contains more
  important texts than block *b*, *c* or *d*. Per default the title and other
  essential texts are shown in block *a*, and block *d* is used for related
  texts (think titles of connected resources).

  The default pivot template uses the following catincluded templates for the
  different blocks:

   * ``pivot/_title_text.tpl`` for block *a*
   * ``pivot/_main_text.tpl`` for block *b*
   * ``pivot/_block_text.tpl`` for block *c*
   * ``pivot/_related_text.tpl`` for block *c*

**title**

  The title used for database sorting etc. Per default the lowercased title in the selected
  pivot language or the default site language.

**related_ids**

  The ids of all related resources and categories. This is a full text index used for
  finding resources that are similar to some other resource.
  All matching resource ids should be prefixed with ``zpo`` and category ids with ``zpc``
  (for example ``zpo1234``).

  The catincluded template ``pivot/_related_ids.tpl`` is used for extracting these ids.
  This template also adds the id of the content group as a related resource.

**address_street, address_city, address_postcode, address_state, address_country**

  The address to be used for searches. Defaults to the related ``address_…`` properties
  with a fallback to the ``mail_address_…`` properties.

  The country should be the two letter ISO code, not the country’s descriptive text.

**name_first, name_surname**

  If the resource is a person then this should contain the first name and surname.

**gender**

  The gender of the person, currently a single letter.  For example: ``f``, ``m`` or ``?``.

**date_start, date_end**

  The start or end date associated with the resource. This should be in a parseable format, which
  will be done automatically when echoing a date directly. Make sure UTC is used as the timezone (note that the timezone will be set to UTC when pivoting).

**date_start_month_day, date_end_month_day**

  A four digit number representing the month and day for the date start and date end. Think of
  an index for birthdays, without exposing the birth year.

  Defaults to ``{{ id.date_start|date:"md" }}`` and ``{{ id.date_end|date:"md" }}``.

**location_lat, location_lng**

  The latitude and longitude of the resource location. This should be empty or a floating point number.

  Defaults to ``{{ id.computed_location_lat|default:id.location_lat }}`` and ``{{ id.computed_location_lng|default:id.location_lng }}``

**date_repivot**

  The date and time this resource should be repivoted. This is useful if some resources (like events) should be indexed with lower priorities after a certain date. A pivot task will be scheduled at the specified date.
  Per default no repivot will be scheduled.


See :ref:`cookbook-custom-pivots` for adding your own custom pivot columns.
