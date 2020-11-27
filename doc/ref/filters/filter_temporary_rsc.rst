.. highlight:: django
.. include:: meta-temporary_rsc.rst

Creates a temporary resource if its input value is not defined.

The created resource receives the properties of the second parameter.

After the resource is created, every hour a check is made if the
resource has been edited and still registered in the Server Storage.

.. note::

    ``mod_server_storage`` must be enabled for this filter to work.

If the resource is abandoned and not changed since its creation, then it is automatically deleted.

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


See :ref:`mod_admin`, :ref:`mod_server_storage`
