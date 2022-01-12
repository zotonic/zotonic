.. _guide-cookbook-frontend-growl:

Enabling Growl Notifications
============================

.. seealso:: :ref:`action-growl`, :ref:`tag-lib`

Using growl outside admin requires some magic to make it work.

Why
---

Growls provide an unobtrusive way of notifying users of background
events or the completion of tasks.  This guide provides step-by-step
instructions on how to enable it for your site.

Assumptions
-----------

Readers are expected to be familiar with template editing.

How
---

Although the magic is quite simple. The missing pieces are a couple of
client side scripts, ``/lib/js/modules/z.notice.js`` and
``/lib/css/z.growl.css`` from :ref:`mod_base`.

In your ``base.tpl`` template, include these files using the :ref:`tag-lib` tag:

.. code-block:: django

  {% lib "js/modules/z.notice.js" %}

And the CSS:

.. code-block:: django

  {% lib "css/z.growl.css" %}

Now you should be able to use growl actions in your templates, example:

.. code-block:: django

  {% button action={growl text="hello world"} %}


