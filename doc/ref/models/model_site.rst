.. highlight:: django
.. include:: meta-site.rst

Retrieve information that is stored in the
:ref:`site configuration <ref-site-configuration>`. If you want to query
values from the config table instead, you should use :ref:`model-config`.

.. note::

   In general the site configurarion is only accessible via the ``m.site``
   template model for users with administrator rights. Exceptions are
   ``{{ m.site.title }}``, hostname configurations and the *paglen*.

Fetch a site configuration key
------------------------------

Example, fetching the site configuration key “hostname”::

    {{ m.site.hostname }}

Fetching all configurations
---------------------------

It is easy to loop over all site configurations::

    {% for key, value in m.site %}
      {{ key }} -- {{ value }} <br>
    {% endfor %}

Overriding config values
------------------------

Zotonic has two places where a site’s configuration is kept. One is in
the site’s config files, the other in the config table. The config
table (accessible through :ref:`model-config`) overrules any module settings
from the config file, for rows where the `module` key of the config value is set
to `site`.

Within the site configuration, you can override module-specific
configuration: Module configurations are defined with a property key
equal to the module name, with a property list as value. For example:

.. code-block:: erlang

    {mod_foo, [ {hostname, "localhost"} ]}

will put the default "hostname" setting of the imaginary ``mod_foo``
module to ``localhost``.


Default config values
---------------------

Sites have the following default config settings:

+---------------------------+-------------------------------------------------+-------------------+
| Property                  | Description                                     | Example value     |
+===========================+=================================================+===================+
| environment               | Set the DTAP status of the site.                | development       |
|                           | Can be one of production, development, test,    |                   |
|                           | acceptance, education or backup.                |                   |
|                           | Default: production                             |                   |
+---------------------------+-------------------------------------------------+-------------------+
| hostname                  | The hostname of the site                        | example.com       |
+---------------------------+-------------------------------------------------+-------------------+
| title                     | The title of the site.                          | "My Awesome Blog" |
+---------------------------+-------------------------------------------------+-------------------+
| protocol                  | The main protocol of the site. Used to          | "https"           |
|                           | construct urls. Default "http".                 |                   |
+---------------------------+-------------------------------------------------+-------------------+
| document_domain           | The document domain used for cross domain       | www.example.com   |
|                           | iframe javascripts. Default is the same as      |                   |
|                           | the cookie_domain.                              |                   |
+---------------------------+-------------------------------------------------+-------------------+
| cookie_domain             | The domain to use on cookies. This defaults     | .example.com      |
|                           | to undefined, which will equal the domain of    |                   |
|                           | the current request.                            |                   |
+---------------------------+-------------------------------------------------+-------------------+
| session_expire_1          | The initial timemout after setting up a process | 10                |
|                           | and waiting for the first ping from the page.   |                   |
|                           | Default: 40 (seconds)                           |                   |
+---------------------------+-------------------------------------------------+-------------------+
| session_expire_n          | Session inactivity timeout after receicing some | 120 (2 minutes)   |
|                           | communication from the page.                    |                   |
|                           | Default: 3600 (1 hour)                          |                   |
+---------------------------+-------------------------------------------------+-------------------+
| session_expire_inactive   | User inactivity timeout after seeing that the   | 3600 (1 hour)     |
|                           | user has not been active.                       |                   |
|                           | Default: 14400 (4 hours)                        |                   |
+---------------------------+-------------------------------------------------+-------------------+
| autologon_expire          | Auto logon cookie timeout setting.              | 31536000          |
|                           | Default: 15552000 (3 months)                    | (365 days)        | 
+---------------------------+-------------------------------------------------+-------------------+
| site                      | The name of the site, an atom.                  | wwwzotonic.       |
+---------------------------+-------------------------------------------------+-------------------+
| hsts                      | Indicate if the site should use Strict          | true              |
|                           | Transport Security. When set, the browser will  |                   |
|                           | no longer use insecure http to access the site. |                   |
|                           | Warning: Be sure your site is accessible via    |                   |
|                           | https before enabeling this feature.            |                   |
|                           | Default: false                                  |                   |
+---------------------------+-------------------------------------------------+-------------------+
| hsts_maxage               | The time, in seconds which browsers are allowed |                   |
|                           | to remember the HSTS setting of the site.       |                   |
|                           | Default: 17280000 (200 days)                    |                   |
+---------------------------+-------------------------------------------------+-------------------+
| hsts_include_subdomains   | When set, the browser also does not use http    |                   |
|                           | for subdomains. Default: false                  |                   |
+---------------------------+-------------------------------------------------+-------------------+
| hsts_preload              | When set, the site's HSTS setting will be       |                   |
|                           | stored by the browser vender. This means that   |                   |
|                           | browsers only use https. Use with care, because |                   |
|                           | it is hard to revert wrong settings.            |                   |
|                           | Default: false                                  |                   |
+---------------------------+-------------------------------------------------+-------------------+
