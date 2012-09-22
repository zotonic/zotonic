Code style conventions
========

**Note**: As these conventions were established only after a large
part of the codebase has been written, the code style described here
is not yet in effect in all parts of the Zotonic code base. We're
trying to gradually adapt to it, however.

Indenting Erlang code
---------------------

We use the "Emacs-style" indenting. This indenting style seems to be a
convention in much of the Erlang world.

Indenting templates
-------------------

### explain how we do this and how we indent HTML vs. template tags? ###

Spaces versus tabs
------------------

We use spaces everywhere. One tab stop is equal to 4 spaces.

When writing code, do not introduce trailing whitespace, do not mix
spaces and tabs and try to keep lines of code shorter than 80
characters.


Writing Commit Messages
-----------------------

### taken from rebar's readme, https://github.com/basho/rebar; I find those quite sensible. ###

Structure your commit message like this::

  One line summary (less than 50 characters)

  Longer description (wrap at 72 characters)

The summary should be less than 50 characters, and tell what was
changed. Use the imperative present tense (fix, add, change). For
example: `Add 'foobar' filter`, `Fix bug in media upload service`.

The description should explain the intention and implementation of
your approach, in the present tense.

When your commit fixes a bug on github, add `Fixes #1545` on a separate line below the description.

Atomicity
---------
Break up logical changes into separate commits. Make whitespace changes separately.
