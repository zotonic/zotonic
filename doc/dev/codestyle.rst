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

### explain how we do this and how we indent HTML vs. template tags? ###


Spaces versus tabs
------------------

We use spaces everywhere. One tab stop is equal to 4 spaces.

When writing code, do not introduce trailing whitespace and try to keep lines
of code shorter than 80 characters.


Writing Commit Messages
-----------------------

The Zotonic commit convention are slightly based on `rebar's README
<https://github.com/basho/rebar>`_.

Structure your commit message like this::

  prefix: One line summary (less than 50 characters)

  Longer description, multiline text that is supposed to wrap at 72
  characters.

  Fixes #403

* **Prefix**: Every commit message must start with one of the designated commit
  prefixes:

 * ``mod_foobar:`` Changes that are related to a single module should
   be prefixed with the module name.
 * ``core:`` For changes in the `src`, `include` or `deps` folder;
   e.g. everything outside modules.
 * ``doc:`` For changes to the documentation, everything below doc/
 * ``scripts:`` for changes to the ``zotonic`` command and its helper scripts.
 * ``tests:`` for unit tests and the testsandbox.
 * ``skel`` for the skeleton sites.
 * ``zotonic_status`` for the default site.

* The **summary** should be less than 50 characters, and tell what was
  changed. Use the imperative present tense (fix, add, change). For
  example: `Add 'foobar' filter`, `Fix bug in media upload service`.

* The description should explain the intention and implementation of
  your approach, in the present tense.

*  Optionally, when your commit fixes a bug on github, add `Fixes #1545` on a separate line below the description.

Notice the empty line preceding the longer description and the "Fixes" tag.


Git best practices
------------------

* Please maintain commit atomicity by breaking up logical changes into
  separate commits; e.g., do not commit unrelated fixes into a single
  commit.

* Make whitespace changes separately.

* When updating from the Zotonic source, please use ``git pull
  --rebase`` to prevent unnecessary merge commits.

* Generally, try to `Mind your Git Manners <http://blog.8thlight.com/kevin-liddle/2012/09/27/mind-your-git-manners.html>`_.


The CONTRIBUTORS file
---------------------

When this is your first contribution to Zotonic, you are welcome to
add your name and e-mail address to the CONTRIBUTORS file in the root
of the project. Please keep the file alphabetically ordered.
