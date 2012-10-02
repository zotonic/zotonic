.. highlight:: django
.. include:: meta-inject_recipientdetails.rst

Adds recipient query string details to hyperlinks.

This filter is meant for use inside e-mail templates. It replaces each
occurrence of ``##`` with the details of the subscriber that the mail
is sent to, encoded as query string arguments.

Each occurrence of ``##`` will be transformed to recipient details in
the following form::

  ?email=foo@bar.com&name_first=John&name_last=Doe

Its use case is when sending a mailing with a link in it which arrives
at a webpage where the user has to enter his e-mail address and name
details. Using this filter, those parameters can be conveniently be
pre-filled.

