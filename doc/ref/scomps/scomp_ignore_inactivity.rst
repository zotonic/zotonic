.. highlight:: django
.. include:: meta-ignore_inactivity.rst

This tag disables zotonic's user activity tracking. Normally zotonic
indicates to the server if the user is active or not. Normally sessions
are stopped after 1 hour of inactivity.

Sometimes however it is important to keep the user session alive. This is
possible with this tag.

Example:

.. code-block:: none

    {# Somewhere in a template #}
    {% ignore_inactivity %}
    ...

This will make sure that the session is kept alive as long is the user
has the page open and there is an internet connection.
