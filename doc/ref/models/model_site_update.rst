
.. include:: meta-site_update.rst

Model for checking if a site has version control enabled.

Also implements the webhook to force a pull of new code from the version control system.

The URL for the webhook is::

    https://yoursite.test/api/model/site_update/post/webhook/<token>

Where ``<token>`` should be replaced with your configured token.

The token can be configured in the admin on System > Modules and
then the config of the mod_site_update module.

The token is saved in the config key ``mod_site_update.webhook_token``.

