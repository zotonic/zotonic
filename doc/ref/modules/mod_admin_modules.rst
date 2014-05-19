
.. include:: meta-mod_admin_modules.rst

Adds support in the admin for activating and deactivating
:term:`modules <Zotonic module>`.

The module overview in the admin presents a list of all modules,
ordered by module status and priority. An activate/deactivate button
allows the module to be (de)activated.

.. image:: /img/mod_admin_modules.png

Module configuration dialog
---------------------------

When activate, some modules have a "configuration" button next to the
activate/deactivate button. In that case, you can click the button to
pop up a configuration dialog where you can set options which are
required to configure the module.

To create such a dialog yourself, include a template called
``_admin_configure_module.tpl`` in your ``templates/`` folder in your
module.
           
