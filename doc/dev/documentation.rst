.. highlight:: none
.. _dev-documentation:

Contributing documentation
==========================

For documentation, we encourage contributions to Zotonic from the
community even more!


We use git for documentation. For large documentation changed, you
should take the same approach as with :doc:`contributing`: e.g. create
a fork of Zotonic, create a topic branch, make the changes, push, pull
request.

However, for small changes, typo's, et cetera, Github provides a nice
edit button which you can use to edit these ``.rst`` files.


Writing documentation
---------------------

Emacs' `rst-mode
<http://docutils.sourceforge.net/docs/user/emacs.html>`_ does the job
for most things. It has nice coloring and indenting.

Heading styles
..............

Use the following convention for headings::

  First-level heading
  ===================

  Second-level heading
  --------------------

  Third-level heading
  ...................


When writing documentation of modules, actions, etc; anything under
``ref/``; the first level heading is already there for you, generated
in the ``meta-*.rst`` file. So you should only use `----------` and
`..........` for the headings in the ``ref/`` files.

  
Table styles
............

For the easy editing of tables, we use Emacs' `table-mode
<http://emacswiki.org/emacs/TableMode>`_, which at first has a bit of
a learning curve but actually works pretty well when creating the
ascii-art tables that the RST format requires you to use.

In general, we use this style of tables::

  +--------------------+-------------------+
  | Header             |Other header       |
  +====================+===================+
  |This is the table   |Some more contents |
  |cell contents       |                   |
  +--------------------+-------------------+

  
