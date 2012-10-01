
.. include:: meta-config.rst

Zotonic has two places where a site’s configuration is kept. One is in
the site’s config file (accessible through :ref:`model-site`), the
other in the config table. Entries in the config table overrule any module
settings from the config file.

All m_config keys can be thought of as tuples ``{Module, Key,
Value}``, where Value is a complex value that can have a text value
but also any other properties. Only configuration with text values can
be edited in admin.

The config model table has different access methods. Below are
examples of what is possible and how to access the configuration.

Fetch the value of a config key
-------------------------------
Example, fetching the config key value of mod_emailer.from_email::

  {{ m.config.mod_emailer.email_from.value }}

Where m.config.mod_emailer.email_from returns a property list which is much like::

  [{id,5},
   {module,<<"mod_emailer">>},
   {key,<<"email_from">>},
   {value,<<"no-reply@example.com">>},
   {props,undefined},
   {created,{{2009,11,7},{20,48,27}}},
   {modified,{{2009,11,7},{20,48,27}}}
  ]

When the config key would have any extra value (besides the value
property) then they would be visible as extra properties and the
property “props” would not have been present.

When the configuration comes from the site config then the id property
is not present.  

Fetching all configurations of a module
---------------------------------------

This only returns configurations from the config table; configuration
keys from the site config are not mixed in. This might change in the
futurre::

  {% for key, value in m.config.mod_emailer %}
      …
  {% endfor %}

Fetching all configurations
---------------------------

This only returns configurations from the config table. Configurations
from the site config are not mixed in. This might change in the
future::

  {% for mod,keys in m.config %}
    {% for key,value in keys %} 
      …
    {% endfor %}
  {% endfor %}

Listening for config changes
----------------------------

m_config uses z_notifier to broadcast events when values are changed or deleted::

  #m_config_update{module=..., key=..., value=...}

for "complex" property value updates/inserts a slighly different notification is used::

  #m_config_update_prop{module=..., key=..., prop=..., value=...}

For config key deletes, the ``#m_config_update{}`` record is broadcast
with ``undefined`` for the value::

  #m_config_update{module=..., key=..., value=undefined}
