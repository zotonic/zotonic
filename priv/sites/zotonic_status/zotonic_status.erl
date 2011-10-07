-module(zotonic_status).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Zotonic Status").
-mod_descr("Default Zotonic site, used when no other site can handle the supplied Host.").
-mod_prio(10).

-export([
    observe_user_is_enabled/2,
    observe_acl_logon/2,
    observe_acl_logoff/2
]).

-include_lib("zotonic.hrl").

%% @doc Check if an user is enabled.
observe_user_is_enabled(#user_is_enabled{id=UserId}, _Context) ->
    UserId == 1.

%% @doc Let the user log on, this is the moment to start caching information.
observe_acl_logon(#acl_logon{id=UserId}, Context) ->
    Context#context{user_id=UserId}.

%% @doc Let the user log off, clean up any cached information.
observe_acl_logoff(#acl_logoff{}, Context) ->
    Context#context{acl=undefined, user_id=undefined}.
