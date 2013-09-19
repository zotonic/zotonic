.. highlight:: django
.. include:: meta-site.rst

Retrieve information which is stored in the site’s `config` files.

The site configuration is stored for each site in
``priv/sites/<sitename>/config`` and files in
``priv/sites/<sitename>/config.d/``. Their syntax is equal to an
Erlang property list, with unique keys.


Fetch a site configuration key
------------------------------

Example, fetching the site configuration key “hostname”::

  {{ m.site.hostname }}

Fetching all configurations
---------------------------

It is easy to loop over all site configurations::

  {% for key, value in m.site %}
      {{ key }} -- {{ value }} <br />
  {% endfor %}

Overriding config values
------------------------

Zotonic has two places where a site’s configuration is kept. One is in
the site’s config files, the other in the config table. The config
table overrules any module settings from the config file, for rows
where the `module` key of the config value is set to `site`.

Within the site configuration, you can override module-specific
configuration: Module configurations are defined with a property key
equal to the module name, with a property list as value. For example::

  {mod_foo, [ {hostname, "localhost"} ]}

will put the default "hostname" setting of the imaginary ``mod_foo``
module to `localhost`.
