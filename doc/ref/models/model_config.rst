.. highlight:: django
.. include:: meta-config.rst

Zotonic has two places where a site’s configuration is kept:

- the site’s :ref:`config file <ref-site-configuration>` (accessible through
  :ref:`model-site`)
- the site’s ``config`` database table. Entries in the config table overrule
  any module settings from the config file.

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

Or, from Erlang::

    m_config:get_value(mod_emailer, email_from, Context).

Where m.config.mod_emailer.email_from returns a property list which is much like::

    [
        {id,5},
        {module,<<"mod_emailer">>},
        {key,<<"email_from">>},
        {value,<<"no-reply@example.com">>},
        {props,undefined},
        {created,{{2009,11,7},{20,48,27}}},
        {modified,{{2009,11,7},{20,48,27}}}
    ]

If the database does not contain a ``mod_email.email_from`` configuration
parameter, Zotonic falls back to the site configuration file and tries to find
the parameter there.

When the config key would have any extra value (besides the value
property) then they would be visible as extra properties and the
property “props” would not have been present.

When the configuration comes from the site config then the id property
is not present.

Fetching all configurations of a module
---------------------------------------

This only returns configurations from the config table; configuration
keys from the site config are not mixed in. This might change in the
futurr:

.. code-block:: django

    {% for key, value in m.config.mod_emailer %}
        …
    {% endfor %}

Fetching all configurations
---------------------------

This only returns configurations from the config table. Configurations
from the site config are not mixed in. This might change in the
future:

.. code-block:: django

    {% for mod,keys in m.config %}
        {% for key,value in keys %}
        …
        {% endfor %}
    {% endfor %}

Listening for config changes
----------------------------

m_config uses z_notifier to broadcast events when values are changed or deleted::

    #m_config_update{module=Module, key=Key, value=Value}

for "complex" property value updates/inserts a slighly different notification is used::

    #m_config_update_prop{module=Module, key=Key, prop=Prop, value=Value}

For config key deletes, the ``#m_config_update{}`` record is broadcast
with ``undefined`` for the value::

    #m_config_update{module=Module, key=Key, value=undefined}
