
.. include:: meta-persistent.rst

This implements persistent storage of values between visits from the same browser.

Persistent storage is implemented using a cookie and database storage. The cookie contains the
key for the stored values. If a visitor makes a request then the session will load the persistent
values into the session process. These can then be accesses using ``{{ m.persistent.yourkey }}`` in
templates (replace yourkey with the key you used to store the information).

The persistent cookie (``z_pid``) is only set if persistent information is saved. This can be done
using calls zo ``z_context:set_persistent/3`` or the action :ref:`action-persistent_set`.

Retrieve a value with ``z_context:get_persistent/2`` or the :ref:`model-persistent`.

Note that the first time a persistent value also a cookie needs to be set. This cookie will be
added to the *Context* passed to the *set_peristent* call. If this was a regular request (HTTP or Ajax)
then the cookie will be returned in the request reply. If the request was a Websocket request then
the user-agent will make a call back to the server to retrieve (and set) the new persistent cookie.

