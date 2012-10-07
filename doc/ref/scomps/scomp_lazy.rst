.. highlight:: django
.. include:: meta-lazy.rst

Custom tag which adds a 'loader' image to the page and performs
a one-time action when loader comes into view.

:ref:`mod_geomap` uses this to load the map Javascript once the admin
widget has been opened by the user.

Example::

  <div id="{{ #lazy }}">
    {% lazy action={update target=#lazy id=id template="_geomap_admin_location_map.tpl"} %}
  </div>
