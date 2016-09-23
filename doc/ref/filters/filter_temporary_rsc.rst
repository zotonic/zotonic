.. highlight:: django
.. include:: meta-temporary_rsc.rst

Creates a temporary resource if its input value is not defined.

The created resource receives the properties of the second parameter.
The resource is tied to the lifetime of the session, if the session stops then the
resource is deleted if it has not been changed in the meantime.

Only a single temporary resource can exist per category per session. Requesting
a second resource of the same category for a session will return the earlier created
resource. If the earlier resource has been updated, then a new resource is created.

This filter is used for situations where an edit form is needed but an intermediate dialog
requesting for the title of the resource is unwanted.

Example:

.. code-block:: none

    {% with id|temporary_rsc:{props title=_"New" category=`article`} as id %}
        {% wire id=#form type="submit" postback=`rscform` delegate=`controller_admin_edit` %}
        <form id="{{ #form }}" action="postback">
            <input type="hidden" name="id" value="{{ id }}" />
            <input type="hidden" name="is_published" value="1" />
            <input type="text" name="title" value="{{ id.title }}" />
            ...
        </form>
    {% endwith %}


See :ref:`mod_admin`
