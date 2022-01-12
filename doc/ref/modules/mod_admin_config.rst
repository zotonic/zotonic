
.. include:: meta-mod_admin_config.rst

Add support for editing the site’s configuration values, as accessed
through :ref:`model-config`.

The page in the admin is a list of every configuration module, key and
textual value. Entries can be added, removed, and edited, if the user
has the permission to do so.

When a value is large than 65 characters, it will be truncated in the
admin config list view. To view the whole value, click the row.


SSL certificates configuration
------------------------------

Via the menu System > SSL Certificates all available certificates can be viewed.

The SSL modules add a panel here using a template ``_admin_config_ssl_panel.[module_name].tpl``.
Only the SSL modules providing a certificate are listed.

Here the module ``mod_ssl_letsencrypt`` lets you request a free certificate from the
LetsEncrypt service.


Email configuration
-------------------

.. seealso:: :ref:`model-config`

Configuration of outgoing email. This module provides a settings page via the admin
menu System > Email configuration, where all email related settings are grouped.

On this page it is possible to send a test email.

Email modules like ``zotonic_mod_mailgun`` provide additional settings panels on
this page. This is possible by adding a template with the name ``_admin_config_email_panel.tpl``.

