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
for most things. It has nice coloring and indenting. Paragraphs are
hard-wrapped at 80 characters with single newlines.


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
in the ``meta-*.rst`` file. So you should only use ``----------`` and
``..........`` for the headings in the ``ref/`` files.


When using Emacs, this little snippet helps with adding underlines to headings::

   (defun underline-with-char (char)
     (interactive (list (read-from-minibuffer "Char: ")))
     (when (= 0 (length char))
       (error "Need a character"))
     (setq char (aref char 0))             ; Ignore everything but the first char.
     (save-excursion
       (goto-char (point-at-eol))
       (insert "\n"
               (make-string (- (point-at-eol)
                               (point-at-bol))
                            char))))

From a mailing list `post <http://lists.gnu.org/archive/html/help-gnu-emacs/2008-05/msg00305.html>`_.


References
..........

Be generous with using references (``:ref:`pagelabel```) in your
writing. The more terms are linked to their respective documentation
pages, the better. Only make the first occurrence of a term a
reference to its page, though; consequent occurrences can be made
```italic```.

Add a ``.. seealso::`` section at the bottom to highlight any other
pages which are closely related to the current one, for example::

  .. seealso:: :ref:`dev-contributing`


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


Writing consistent Cookbook items
---------------------------------  

A Zotonic Cookbook item is a single-concept solution to a well-defined
problem, living in the :ref:`manual-cookbook` section of the
documentation.

Useful items range from the simplest content management tasks to
technically sophisticated module development and site administration
solutions. This means that items are welcomed from noobies and wizards
alike.

Whenever you struggle to find a solution to a specific problem, fail
to find a Cookbook item that addresses it, and work through the
solution with a final "Aha!," you have the raw material for an
excellent Cookbook submission.

A well-written item has four sections:

**WHY**: What problem does this Cookbook item solve? What benefits does it deliver?

Four major reasons for submitting Cookbook items are: 

1. The best way to learn is to teach

2. Your Cookbook items documents your efforts; helps you remember what
   you did next time you encounter a similar problem

3. Each item makes it that much easier for noobies and other community
   members to advance their Zotonic skills.

**ASSUMPTIONS**: What does this item assume about operating system, Linux distribution, programming skills, knowledge of Zotonic architecture and conventions, etc.

**HOW**: Step-by-step instructions for implementing your solution.

Don't take user competency for granted. When you specify a command,
note what user name you're working under and what directory you're
working in.  Respect the noobies by including steps that may be
obvious to you but not so obvious to folks with less experience.

Think of your instructions as a check-list. A noobie should be able to
achieve success by reading, implementing and checking off each
instruction.  Keep your instructions simple, complete, and clear.

Recruit a noobie to try out your solution. Fix the stumbling blocks
she encounters. If you can't find a noobie, put yourself in noobie
mind. Remember, you too once were one.






  
.. seealso:: :ref:`dev-contributing`

   
