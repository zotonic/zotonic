.. highlight:: django
.. include:: meta-render.rst

Render a template.

Example::

  {% include "_email.tpl" name="_name.tpl"|render:%{ id: recipient_id } %}

This renders the template ``_name.tpl`` with the argument ``id``.

If the argument is only single id, then it can be passed at once and will be
assigned to the ``id`` argument. The following is equivalent to the example
above::

  {% include "_email.tpl" name="_name.tpl"|render:recipient_id %}

