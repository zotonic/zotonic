.. highlight:: django
.. include:: meta-mailinglist_subscribe.rst

Show the mailinglist subscription form to subscribe to a certain
mailinglist id.

Parameters:

``id``
  Required; the id of the mailinglist :term:`resource` that is being subscribed to.

``template``
  Which form template to render. Defaults to the template ``_scomp_mailinglist_subscribe.tpl``.

All other parameters are passed in to the template which is being rendered.

The form is at least supposed to have an `email` input field. Besides
`email`, it can have `name_first`, `name_surname_prefix` and
`name_surname` fields, which will be stored in the recipient table.
