.. include:: meta-lazy.rst

lazy
====

scomp which shows a 'loader' image and performs onetime actions when loader comes into view.

:ref:`module-geomap` uses this to load the map javascript once the admin widget has been opened by the user.

Example::

  <div id="{{ #lazy }}">
    {% lazy action={update target=#lazy id=id template="_geomap_admin_location_map.tpl"} %}
  </div>
