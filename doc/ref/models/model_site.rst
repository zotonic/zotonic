.. highlight:: django
.. include:: meta-site.rst

Retrieve information which is stored in the site's `config` file.

The site configuration is stored for each site in
``priv/sites/<sitename>/config``. Its syntax is equal to an Erlang
property list. Some properties can be repeated, for example the
“hostalias” property, to specify multiple host aliases.


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

Fetching all values of a repeating config
-----------------------------------------

Some config keys have multiple values. To fetch all values of, for
example, the “hostalias” key use::

  {{ m.site.all.hostalias }}


Overriding config values
------------------------

Zotonic has two places where a site’s configuration is kept. One is in
the site’s config file, the other in the config table. The config
table overrules any module settings from the config file, for rows
where the `module` key of the config value is set to `site`.

Within the site configuration, you can override module-specific
configuration: Module configurations are defined with a property key
equal to the module name, with a property list as value. For example::

  {mod_foo, [ {hostname, "localhost"} ]}

will put the default "hostname" setting of the imaginary ``mod_foo``
module to `localhost`.
