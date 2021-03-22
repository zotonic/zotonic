
.. include:: meta-mod_microsoft.rst

Adds logon using the Microsoft identity platform.

If enabled then on ``/admin/authentication-services`` a panel is added for the Microsoft identity platform.

Here the following can de configured:

 * Application ID: as found in the app registration
 * Client Secret: as found in the app registration
 * Scope: space separated list of scopes that you want the user to consent to. Examples are: email, offline_access and profile. The openid scope is always added automatically. Defaults to ``email profile``.
 * Tenant: Control who can sign in. Allowed values are: common, organizations, consumers, and tenant identifiers. Defaults to ``common``.


On the `Azure Portal App registrations <https://go.microsoft.com/fwlink/?linkid=2083908>`_ an App can be registered and configured.

The redirect path for the app is shown on top of the ``/admin/authentication-services`` screen and is of the format: ``https://example.com/oauth-service/redirect``


