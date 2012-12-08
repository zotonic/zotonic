
.. include:: meta-mod_oauth.rst

Module providing OAuth authentication support for the
:ref:`manual-services` of Zotonic.

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

The module adds an admin menu for defining OAuth applications
(`consumers`). For each consumer, you can specifiy which OAuth calls
are permitted.

.. seealso:: :ref:`controller-api`

.. todo:: Add more documentation
