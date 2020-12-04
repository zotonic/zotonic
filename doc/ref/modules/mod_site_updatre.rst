
.. include:: meta-mod_site_update.rst

This module pulls updates of a site‘s code from a remote version control system.
The supported version control systems are git and mercurial.

After enabling this module you will see a button **Update site** on the System -> Status page in the admin.

This button is only available to users with the *use.mod_site_update* right.


After a pull of new code, Zotonic will do everything that is needed. Including compilation
of source code, reloading of dispatch rules, reloading of translations and reindexing the
template directories.

This update of the system can take a while.

Webhook
-------

In GitHub and other systems it is possible to set a *webhook* that is called when new updates
are pushed to the repository.

In System > Modules there is a *Config* button next to the mod_site_update module.
Click this and configure a secure token to be passed to the webhook.

The URL for GitHub et al is::

    https://yoursite.test/api/model/site_update/post/webhook/<token>

Where ``<token>`` should be replaced with your configured token.

The token is saved in the config key ``mod_site_update.webhook_token``.

Ensure that the external system performs a HTTP POST to the webhook, the posted body
is dropped.


Zotonic status site
-------------------

The ``mod_site_update`` module is also used by the zotonic status site.

Here the module adds buttons to update sites and a button to update the complete Zotonic system.

Site updates get their datamodel fixed by automatic restarts of modules, ensure that you
increment the ``-mod_schema(...)`` version number.

Zotonic updates are a bit more complicated. Small updates will be fine. Large updates that
also change the datamodel or add new Erlang dependencies need more compilation and a system
restart. So be careful with using this update button.
