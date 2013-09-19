
.. include:: meta-geomap_static.rst

Shows a location’s map using static images from OpenStreetMap.

The location is taken from the tag’s `latitude` + `longitude`
parameters, or, when absent, from the `id` parameter which is supposed
to be a :term:`resource` of category `location` (e.g. having an
address).

It displays the ``_geomap_static.tpl`` template.

Other parameters:

``zoom``
  Zoom level (defaults to 14)

``n``
  How many rows and cols to display (defaults for the `rows` and `cols` parameters), defaults to 2.

``cols``
  How many grid columns to display

``rows``
  How many grid rows to display

``size``
  The size in pixels of each tile, defaults to 256.
