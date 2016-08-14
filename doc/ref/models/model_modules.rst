
.. include:: meta-modules.rst

Access information about which :ref:`modules <guide-modules>` are installed
and which ones are active.

To test if a module is activated, for instance mod_signup::

    {% if m.modules.active.mod_signup %}
        {# Do things that depend on mod_signup #}
    {% endif %}

To print a list of all active modules::

    {{ m.modules.all|pprint }}
