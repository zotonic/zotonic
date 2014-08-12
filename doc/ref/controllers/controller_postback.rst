
.. include:: meta-postback.rst

The postback controller is the endpoint for AJAX callbacks and transports from
the browser to the server.

See :ref:`manual-transport` for more information about transporting data between the server and the browser.

This controller is used internally by :ref:`scomp-wire` and :ref:`action-postback`.


HTML form posts
---------------

It is possible to directly post HTML forms to the postback controller.

.. highlight:: html

The method of the form must be POST, the action has two possibilities::

    <form method="POST" action="/postback/mymessage">...</form>

or::

    <form method="POST" action="/postback/mymessage/mymodule">...</form>

.. highlight:: erlang

The first example will call::

    z_notifier:first(#submit{message="mymessage"}, Context).

The second example will call the mentioned module directly::

    MyModule:event(#submit{message="mymessage"}, Context).

All posted query arguments are available via the usual ``z_context:get_q("arg", Context)`` calls.

Actions and other generated javascript can only returned to the browser if the ``z_page_id`` was included in the post.
Body content and headers can be set in the *Context* and will be sent back to the browser.


.. todo:: Extend documentation
