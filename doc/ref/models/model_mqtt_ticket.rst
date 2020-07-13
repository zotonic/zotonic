.. highlight:: django
.. include:: meta-mqtt_ticket.rst

A ticketing system for out of band MQTT posts via HTTP.

The controller :ref:`controller-mqtt_transport` can accept HTTP posts.
These posts MUST include a ticket and a topic for the payload to be sent.

The ticket is obtained via ``model/mqtt_ticket/post/new``. This can only
be accessed via MQTT, as the routines will check for a valid ``client_id``.

If a ticket is created then the current request context is saved for
max 30 seconds. If a request comes in then the ticket is used to fetch
the stored context and the following information is copied over from
the stored context to the HTTP request context:

 * MQTT client-id
 * MQTT client-topic
 * MQTT routing-id
 * ACL user id
 * ACL auth_options
 * ACL read-only flag
 * Timezone
 * Language

A ticket can only be used once.

