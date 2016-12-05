.. highlight:: django
.. include:: meta-is_letsencrypt_valid_hostname.rst

Test if a hostname can be used for a Let’s Encrypt certificate.

Criteria are:

 1. Does resolve using DNS
 2. The resolved address is not a LAN address
 3. The address is reachable
 4. And the current site is listening for the hostname on that address

The site must listen on port 80 for connections.

For example, check if the current site is reachable as *example.com*::

  {% if "example.com"|is_letsencrypt_valid_hostname %}
      Wow, this site is example.com!?!
  {% endif %}

.. seealso:: :ref:`mod_ssl_letsencrypt`
