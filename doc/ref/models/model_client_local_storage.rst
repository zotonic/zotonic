
.. include:: meta-client_local_storage.rst

Model to access the ``localStorage`` on the client (browser).

The client-id and routing topic in the *context* must be set when calling
the functions in this module. This is the case for all MQTT topic calls
from the client.

The localStorage on the client is accessed via publishing to the topic
``~client/model/localStorage/...``. In the client the topic is ``model/localStorage/...``.

