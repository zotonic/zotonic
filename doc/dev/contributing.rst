.. _dev-contributing:

Contributing source code
========================

We encourage contributions to Zotonic from the community!

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
 * ``doc:`` For changes to the documentation, everything below doc/
 * ``scripts:`` for changes to the ``zotonic`` command and its helper scripts.
 * ``build:`` for the build system and related changes.
 * ``tests:`` for unit tests and the testsandbox.
 * ``skel:`` for the skeleton sites.
 * ``zotonic_status:`` for the default site.
 * ``translation:`` for new/updated translations.
 * ``core:`` For changes in the `src`, `include` or `deps` folder;
   e.g. anything not covered by another tag.

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
