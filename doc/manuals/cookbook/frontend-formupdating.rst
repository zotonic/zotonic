.. _manual-cookbook-frontend-formupdating:

Updating form field from a dialog
=================================

Ever wanted to update a form field from a dialog, possibly giving the
user some list to choose from? Here's how to do it.

Why
---

Some interactions benefit from presenting a dialog for input.  This
guide explains how to take input in a dialog and have it apply to a
form input value.

Assumptions
------------

Readers are expected to understand DOM IDs for uniquely identfying
form inputs and also understand advanced usage of Zotonic Template
tags.

How
---

To be able to update a form field, you need to pass the id of the
element you want to update to the dialog. It may look something like::

  {% button text="Update field dialog" 
          action={dialog_open 
                  title="Update form field value" 
                  template="my_dialog.tpl" 
                  target_id="input_element_id"} 
  %}

Then create your dialog template, my_dialog.tpl in this example, and wire a set_value action to update the input form element::

  <a id="my_anchor" href="javascript:void(0)" href="javascript:void(0)">Click here to update form value</a> 
  {% wire id="my_anchor" 
          type="click" 
          action={set_value target=target_id value="My new value"} 
          action={dialog_close} 
  %} 

If you include a template like the one above into your dialog template
many times (i.e. from a for loop), then having fixed id's are no
good. To prefix the id with a unique value (per invocation of the
template) prefix the id with a ``#``-sign. so the a-tag becomes ``<a
id={{#my_anchor}}…`` and the wire becomes ``wire id=#my_anchor…`` which will
expand to something like ``"ubifgt-my_anchor"``.


.. seealso:: :ref:`manual-template-autoids`

