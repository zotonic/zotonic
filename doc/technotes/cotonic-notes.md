# Cotonic

Notes about the cotonic integration into Zotonic.

## Changes

 * Removed sessions
 * Removed page sessions
 * Added MQTT session (using `mqtt_sessions`)
 * All page transport is now MQTT
 * Authentication via MQTT
 * Postbacks etc are routed via MQTT
 * All transport encoding is now JSON (using `jsonrecord`)


## Authentication / logon

To keep compatibility with Zotonic page changes we need to have authentication between
page loads. The easiest method is to add a cookie for this.

On MQTT connect we could then check this cookie for additional authentication, besides the
authentication in the MQTT `connect` message. This should only be done iff there is no
username/password sent with the connect, and could be handled by the controller_mqtt_transport.

Proposed happy flow:

 1. Fill in credentials in form
 2. Reconnect MQTT using the given username/password
 3. Return a short lived token with the connack properties
 4. Exchange this token with a session cookie (http only) using an api call
 5. Ping a topic that we have a valid authenticated session
 6. Periodically call the token API to extend the session token lifetime

On this topic we optionally have a subscriber to do things like:

 * reload the page
 * update elements on the page

