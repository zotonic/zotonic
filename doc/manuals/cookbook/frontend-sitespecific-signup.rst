.. highlight:: erlang

Site-specific signup actions
============================

Performing additional, project-specific actions when a user signs up

Why
---

When a user signs up, Zotonic verifies the validity of some
information, such as the e-mail address and the password strength. If
these verifications succeed, it stores the user data in the Zotonic
database. If you have your own database with project-specific user
data, you may want to insert additional information into this database
on sign up. To do this, you need to modify the ``sitename.erl`` file,
where `sitename` is the name of your site. Remember that this file is
located in ``priv/sites/sitename/``.

Assumptions
-----------

Readers are assumed to be comfortable with Erlang syntax and use of the command shell.

How
---

Open ``priv/sites/sitename/sitename.erl`` with your favorite editor.

Example 1: a gaming website with several point accounts for each user. Add the following code::

  -export([observe_signup_done/2]).

  %% @doc Perform additional actions when a user signs up
  observe_signup_done({signup_done, UserId, _IsVerified, _Props, _SignupProps},
          Context) ->
      % Create the necessary accounts for the user:
      create_user_accounts(UserId, [chess, go, checkers]).

  % Implementation not shown here
  create_user_accounts(_UserId, _Context) ->
      todo.

Save your changes and quit

Recompile zotonic::

  $ cd /path/to/zotonic 
  $ make 

Now you'll see that when a signup is done successsfully, your ``create_user_accounts/2`` function will be called.
