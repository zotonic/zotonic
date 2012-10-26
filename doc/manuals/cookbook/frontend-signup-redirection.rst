.. highlight:: erlang

Managing redirection after login and signup
===========================================

Configure mod_signup to redirect to something other than a member's home page.

Why
---

The default behavior of Zotonic is to redirect the user to his or her
own page after logon (/page). If you want to change this, you need to
modify the ``sitename.erl`` file, where `sitename` is the name of your
site. Remember that this file is located in ``priv/sites/sitename/``.

Assumptions
-----------

Readers are assumed to be comfortable with Erlang syntax and use of the command shell.

How
---
Open priv/sites/sitename/sitename.erl with your favorite editor::

  $ cd /path/to/zotonic 
  $ vim priv/sites/sitename/sitename.erl 

Example 1
.........

Redirection to the /welcome page after signup and to the /welcome-back page after logon

Add the following code::

  -export([observe_signup_confirm_redirect/2, observe_logon_ready_page/2]).
  
  %% @doc Set the page that must be opened after signup
  observe_signup_confirm_redirect({signup_confirm_redirect, _UserID}, _Context) ->
      "/welcome".
  
  %% @doc Set the page that must be open after login
  observe_logon_ready_page({logon_ready_page, _}, _Context) ->
      "/welcome-back". 

Example 2
..........

Conditional redirection, where the site administrator is redirected to
the /admin page, and all other users are redirected to the /welcome
page::

  -export([observe_logon_ready_page/2]). 
  
  %% @doc Set the page that must be opened after logon
  observe_logon_ready_page({logon_ready_page, _}, Context) -> 
      case z_acl:user(Context) of 
          ?ACL_ADMIN_USER_ID  -> "/admin"; 
        _                   -> "/welcome" 
    end. 

Example 3
.........

Conditional redirection with one option being the default behaviour::

  -export([observe_logon_ready_page/2]). 
  
  %% @doc Set the page that must be opened after logon
  observe_logon_ready_page({logon_ready_page, _}, Context) -> 
      case z_acl:user(Context) of 
          ?ACL_ADMIN_USER_ID  -> "/admin"; 
          _                   -> undefined     % All users except the administrator are redirected to their own pages 
      end. 

      
Example 4
.........

Conditional redirection, where the site administrator is redirected to
the /admin page and all other users are redirected to the /welcome
page, unless another URL is specified::

  -export([observe_logon_ready_page/2]).
  
  %% @doc Set the page that must be opened after logon
  observe_logon_ready_page({logon_ready_page, Url}, Context) ->
      case {z_acl:user(Context), Url} of
          {?ACL_ADMIN_USER_ID, _} ->
              "/admin";
          {_, []} ->
              "/welcome";
          _ ->
              Url
      end.

Save your changes and quit.

Recompile zotonic::

  $ cd /path/to/zotonic 
  $ make 

Now when logging in, you should see this redirect behaviour in action.
