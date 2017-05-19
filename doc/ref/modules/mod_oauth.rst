.. _module-mod_oauth:
.. include:: meta-mod_oauth.rst


Module providing OAuth authentication support for the
:ref:`guide-services` of Zotonic.

OAuth allows resource owners (admins, developers) to authorize third-party access to
their content or modules without sharing their credentials.

If you need to provide API access to one of your own modules, your first step will
be to create a service (sub)module. See :ref:`guide-services` for details.

How the OAuth module operates
-----------------------------

This module hooks in the ``service_authorize`` callback to check
requests handled by :ref:`controller-api` for OAuth authentication
tokens.

Requests processed by this module are checked for the ``Authorization:
OAuth`` request header and if it is found, it checks for a valid token
and verifies the request signature. When this is all done, it looks
which OAuth application (the `consumer key`) is being used and checks
if the requested API service is in the list of allowed services for
that particular application.


Defining OAuth applications
---------------------------

The module adds an admin menu *Auth > API access* for defining OAuth applications
(`consumers`). For each consumer, you can specifiy which OAuth calls
are permitted.


Anonymous access
................

It is possible to allow *Anonymous access* for a consumer.
This enables a client to access the API using only the consumer key and secret.

The client will be granted access, though no user will be logged in for the request.
Because of that the API call will be handled with the same ACL rights as an *anonymous* visitor.


User access
...........

To give an user access, the user will need to register the consumer key and secret with their
client and then perform the *OAuth authentication* using the web redirects.

The client needs to use the follwing URLs:

 * Request token: ``http://example.com/oauth/request_token``
 * Access token: ``http://example.com/oauth/access_token``

After successful authentication a user-specific token and secret will be stored in the database.
The token and first part of the secret are shown in the admin.

.. seealso:: :ref:`controller-api`

.. todo:: Add more documentation
