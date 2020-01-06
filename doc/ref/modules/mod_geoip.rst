
 .. include:: meta-mod_geoip.rst
 .. highlight:: erlang

 Used to map IP addresses to geographical locations.

 This modules uses the freely available MaxMind databases for the IP mapping.
 The database is updated automatically.

 For downloading the database you need a License Key from MaxMind.
 The License Key can be generated after `registering with MaxMind <https://www.maxmind.com/en/geolite2/signup>`_.

 The obtained license key can be configured in the ``erlang.config`` file with::

     {locus, [
         {license_key, "YOUR_LICENSE_KEY"}
     ]}

 The above license key can be overruled in the ``zotonic.config`` file using the ``maxmind_license_key`` key.

 .. note::

     As the database is only once downloaded per Zotonic instance the license key is shared
     between all sites. It is not possible to have a specific per-site key.