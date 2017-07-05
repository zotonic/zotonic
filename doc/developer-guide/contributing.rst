.. _dev-contributing:

Contributing to Zotonic
=======================

We encourage contributions to Zotonic from the community! This chapter describes
how you can help improve Zotonic.

1. Fork the ``zotonic`` repository on Github (at https://github.com/zotonic/zotonic).

2. Clone your fork or add the remote if you already have a clone of
   the repository::

     git clone git@github.com:yourusername/zotonic.git

   or::

     git remote add mine git@github.com:yourusername/zotonic.git

3. Create a topic branch for your change::

     git checkout -b some-topic-branch

4. Make your change and commit. Use a clear and descriptive commit
   message, spanning multiple lines if detailed explanation is needed.

5. Push to your fork of the repository and then send a pull-request
   through Github::

     git push mine some-topic-branch

6. A Zotonic committer will review your patch and merge it into the
   main repository or send you feedback. The pull request page on
   github is the main place to discuss the code and related topics.

.. _dev-codingstyle:

Coding standards
----------------

The Zotonic code follows `Inaka’s Erlang Coding Guidelines`_. This is enforced
using the `Elvis`_ code checking tool. You can check the code locally with:

.. code-block:: bash

    $ elvis rock

When contributing, simply try to follow the coding style as you find it in the
existing code.

Emacs
^^^^^

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

Writing commit messages
-----------------------

The Zotonic commit convention are slightly based on `rebar’s README
<https://github.com/basho/rebar>`_.

Structure your commit message like this:

.. code-block:: none

    prefix: One line summary (less than 50 characters)

    Longer description, multiline text that is supposed to wrap at 72
    characters.

    Fixes #403

* **Prefix**: Every commit message must start with one of the designated commit
  prefixes:

 * ``mod_foobar:`` Changes that are related to a single module should
   be prefixed with the module name.
 * ``doc:`` For changes to the documentation, everything below doc/
 * ``scripts:`` for changes to the ``zotonic`` command and its helper scripts.
 * ``build:`` for the build system and related changes.
 * ``tests:`` for unit tests and the testsandbox.
 * ``skel:`` for the skeleton sites.
 * ``zotonic_status:`` for the default site.
 * ``translation:`` for new/updated translations.
 * ``core:`` For changes in the `apps/zotonic_core` directory; i.e., anything
   not covered by another tag.

* The **summary** should be less than 50 characters, and tell what was
  changed. Use the imperative present tense (fix, add, change). For
  example: `Add 'foobar' filter`, `Fix bug in media upload service`.

* The **description** should explain the intention and implementation
  of your approach, in the present tense.

* Optionally, when your commit **fixes** a bug on github, add `Fixes
  #1545` on a separate line below the description.

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

Running the tests
-----------------

Zotonic comes with a basic test suite which can be run the following way:

.. code-block:: bash

    zotonic runtests

This starts the Zotonic system and executes all EUnit tests. It will
disable all zotonic sites except for the special site ``testsandbox``,
which will be enabled.

The ``testsandbox`` site does not have a database configuration and is
configured to run on ``localhost:8040``.

Contributing documentation
--------------------------

Build the documentation
^^^^^^^^^^^^^^^^^^^^^^^

First, install `Sphinx <http://www.sphinx-doc.org/en/stable/install.html>`_.
To build the documentation, Erlang must be installed.

.. code-block:: bash

    $ cd doc/

    # Install dependencies
    $ pip install -r requirements.txt

    # Generate meta-*.rst files:
    $ make stubs

    # Then generate HTML files:
    $ make html

Then view the HTML files in ``doc/_build/html/index.html``.

Heading styles
^^^^^^^^^^^^^^

Use the following `convention <http://www.sphinx-doc.org/en/stable/rest.html#sections>`_
for headings:

.. code-block:: rst

    First-level heading
    ===================

    Second-level heading
    --------------------

    Third-level heading
    ^^^^^^^^^^^^^^^^^^^

    Fourth-level heading
    """"""""""""""""""""

When writing documentation of modules, actions, etc.; anything under
``ref/``; the first level heading is already there for you, generated
in the ``meta-*.rst`` file. So you should only use ``----------`` and
``..........`` for the headings in the ``ref/`` files.


When using Emacs, this little snippet helps with adding underlines to
headings:

.. code-block:: common-lisp

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

From a mailing list `post
<http://lists.gnu.org/archive/html/help-gnu-emacs/2008-05/msg00305.html>`_.

References
^^^^^^^^^^

Be generous with using references (``:ref:`pagelabel```) in your
writing. The more terms are linked to their respective documentation
pages, the better. Only make the first occurrence of a term a
reference to its page, though; consequent occurrences can be made
```italic```.

Add a ``.. seealso::`` section at the bottom to highlight any other
pages which are closely related to the current one, for example::

.. code-block:: none

    .. seealso:: :ref:`dev-contributing`

Table styles
^^^^^^^^^^^^

For the easy editing of tables, we use Emacs' `table-mode
<http://emacswiki.org/emacs/TableMode>`_, which at first has a bit of
a learning curve but actually works pretty well when creating the
ascii-art tables that the RST format requires you to use.

In general, we use this style of table::

  +--------------------+-------------------+
  | Header             |Other header       |
  +====================+===================+
  |This is the table   |Some more contents |
  |cell contents       |                   |
  +--------------------+-------------------+

Writing consistent Cookbook items
.................................

A Zotonic Cookbook item is a single-concept solution to a well-defined
problem, living in the :ref:`guide-cookbook` section of the
documentation.

Useful items range from the simplest content management tasks to
technically sophisticated module development and site administration
solutions. This means that items are welcomed from noobies and wizards
alike.

Whenever you struggle to find a solution to a specific problem, fail
to find a Cookbook item that addresses it, and work through the
solution with a final “Aha!,” you have the raw material for an
excellent Cookbook submission.

A well-written item has four sections:

**WHY**: What problem does this Cookbook item solve? What benefits
does it deliver?

Four major reasons for submitting Cookbook items are:

1. The best way to learn is to teach

2. Your Cookbook items documents your efforts; helps you remember what
   you did next time you encounter a similar problem

3. Each item makes it that much easier for noobies and other community
   members to advance their Zotonic skills.

**ASSUMPTIONS**: What does this item assume about operating
system, Linux distribution, programming skills, knowledge of Zotonic
architecture and conventions etc.

**HOW**: Step-by-step instructions for implementing your solution.

Don't take user competency for granted. When you specify a command,
note what user name you’re working under and what directory you are
working in.  Respect the noobies by including steps that may be
obvious to you but not so obvious to folks with less experience.

Think of your instructions as a check-list. A noobie should be able to
achieve success by reading, implementing and checking off each
instruction. Keep your instructions simple, complete, and clear.

Recruit a noobie to try out your solution. Fix the stumbling blocks
s/he encounters. If you can’t find a noobie, put yourself in noobie
mind. Remember, you too once were one.

.. _releases:

Zotonic releases
----------------

Release dates
^^^^^^^^^^^^^

Zotonic follows a time-based release model. Every first Monday of the month – at
the call of the `Dutch test siren`_ – a new Zotonic version is released. Version
numbers are incremented according to the `Semantic versioning`_ specification.

Release schedule
^^^^^^^^^^^^^^^^

Preparation for each release lasts one month:

1. **Development phase**: new features are added and existing ones improved.
   Commits take place on the current ``.x`` development branch (for instance,
   ``0.x``).

2. **Stabilisation phase**: five working days before a release, we create a
   release branch from the development branch, incrementing the minor version
   number (for instance, ``release-0.16.0``). During the stabilisation phase, no
   new features are added. Instead, the last bug fixes for the release are
   committed.

3. On the first Monday of each month, the release branch is **tagged**
   (for instance, ``0.16.0``), merged back into the development branch and then
   discarded.

Hotfix releases
^^^^^^^^^^^^^^^

Some bug fixes, such as security fixes, need to be made available immediately.
In case a change cannot wait for the next monthly release, we release it as a
hotfix, incrementing the patch number (for instance, ``0.16.1``).

.. seealso::
    `GitHub <https://github.com/zotonic/zotonic/releases>`_ for the latest
    release.

.. _Dutch test siren: http://www.invadingholland.com/guides-to-holland/emergency-alarm
.. _Semantic versioning: http://semver.org/
.. _Inaka’s Erlang Coding Guidelines: https://github.com/inaka/erlang_guidelines
.. _Elvis: https://github.com/inaka/elvis
