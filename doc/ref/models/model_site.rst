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
      {{ key }} -- {{ value }} <br />
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
