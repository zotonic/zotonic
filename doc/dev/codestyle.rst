.. highlight:: none
.. _dev-codingstyle:

Code style conventions
======================

.. note:: 
   As these conventions were established only after a large
   part of the code base has been written, the code style described here
   is not yet in effect in all parts of the Zotonic code base. We're
   trying to gradually adapt to it, however.


Indenting Erlang code
---------------------

We use the "Emacs-style" indenting (using ``erlang-mode`` provided with the 
Erlang distribution). This indenting style seems to be a convention in much
of the Erlang world.


Indenting templates
-------------------

Provided with the Zotonic distribution is a Zotonic template mode,
``zotonic-tpl-mode``, which supports the Zotonic flavor of ErlyDtl.
It is located in the :file:`priv/emacs/zotonic-tpl-mode.el` file, and
may be installed in emacs by adding something like this to your `.emacs`
file:

.. code-block:: common-lisp

   (add-to-list 'load-path ".../path/to/zotonic/priv/emacs")
   (require 'zotonic-tpl-mode)
   ;; optional, for associating .tpl files with zotonic-tpl-mode
   (add-to-list 'auto-mode-alist '("\\.tpl$" . zotonic-tpl-mode))


Hint when using the m.rsc model
...............................

A common style and shorthand techniques make templates from multiple
authors more alike.  This makes them easier to maintain and share in a
team or with the community.

When the template sees that you request a property of an integer then
it assumes that the integer is a ``m.rsc`` `id`. This makes templates more
readable.

Example:

.. code-block:: django

  <li>
      <h3>{{ m.rsc[id].title }}</h3>
      {% for image_id in m.rsc[id].o.depiction %}
      <figure>
          {% media image_id width=100 link=id %}
          {% if m.rsc[image_id].summary %}
              <p class="image-caption">{{ m.rsc[image_id].summary }}</p>
          {% endif %}
      </figure>
      {% endfor %}
  </li>

Can be more effectively written as follows to improve readability:

.. code-block:: django

  <li>
      <h3>{{ id.title }}</h3>
      {% for image_id in id.o.depiction %}
      <figure>
          {% media image_id width=100 link=id %}
          {% if image_id.summary %}
              <p class="image-caption">{{ image_id.summary }}</p>
          {% endif %}
      </figure>
      {% endfor %}
  </li>


Spaces versus tabs
------------------

We use spaces everywhere. One tab stop is equal to 4 spaces.

When writing code, do not introduce trailing whitespace and try to keep lines
of code shorter than 80 characters.


Writing Commit Messages
-----------------------

See the section in :ref:`dev-contributing` on how to format your
commit messages.
