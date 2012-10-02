.. highlight:: django
.. include:: meta-gravatar_code.rst

Calculate the gravatar code for an e-mail address::

  {{ "arjan@scherpenisse.net"|gravatar_code }}

Will output::

  3046ecab06c4f9cdb49963a96636e5ef

This hash can then be used for `displaying Gravatar images <https://en.gravatar.com/site/implement/hash/>`_.

