
.. include:: meta-mqtt_transport.rst

Controller for transport of MQTT data between the browser (client) and server.

This controller accepts Websocket connections and out-of-band HTTP POSTs.

The HTTP POSTs must have a valid ticket. See :ref:`model-mqtt_ticket` on how to
obtain such a ticket and more information about tickets.

The Websocket connection is a normal MQTT transport over Websocket.

For the authentication the Websocket accepts two methods:

 * Cookie authentication with ``z.auth`` cookie, see :ref:`controller-authentication`.
 * Username and password in the MQTT connect
