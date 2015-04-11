
.. include:: meta-update_iframe.rst

Updates the content of an iframe with a template or a literal HTML text.

.. note:: 
   This action is only used to update an ``iframe`` element.
   Use the :ref:`action-update` action for updating the contents of a normal HTML element.

Example::

   <iframe id="preview"></iframe>
   {% button text="Show Email" action={update_iframe target="preview" template="email.tpl" recipient_id=m.acl.user} %}

When clicked, the contents of the iframe will be set to the rendered ``email.tpl`` template. This replaces any content present.


===========  ===============================================================  ==========================
Argument     Description                                                      Example
===========  ===============================================================  ==========================
target       The id of the iframe receiving the rendered HTML document.       `target="my-view"`
text         Literal HTML doc to be inserted, no escaping will be done.       `text="<html>...</html>"`
template     Name of the template to be rendered.                             `template="page.tpl"`
catinclude   Add this argument to use a :ref:`tag-catinclude` instead of      `catinclude id=1`
             a normal include of the template. The `id` argument *must*
             be present for a catinclude to work.
===========  ===============================================================  ==========================

All other arguments are passed as-is to the included template(s).

.. seealso:: action :ref:`action-update`.
