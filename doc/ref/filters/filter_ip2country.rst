
.. include:: meta-ip2country.rst

Maps an IP address to a country using the MaxMind GeoIP2 database.

The module ``mod_geoip`` must be enabled to use this filter.

Example, print the country code of the current visitor::

  {% print m.req.peer|ip2country %}

Might print ``nl``.

If the IP address could not be mapped then ``undefined`` is returned.
