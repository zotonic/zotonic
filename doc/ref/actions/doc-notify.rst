
.. versionadded:: 0.8

Send a zotonic notify message. All modules which observe this message are notified.

Example::

   {% button action={notify message=`clicked`} %}

Sends the message `clicked` to the notify system. All modules which are subscribed to the `clicked` message are notified.

`Edit <https://github.com/zotonic/zotonic/edit/master/doc/ref/actions/doc-notify.rst>`_
