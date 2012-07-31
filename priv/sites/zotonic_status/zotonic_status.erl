%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell <marc@worrell.nl>
%% Date: 2011-12-23

%% @doc Default Zotonic site, used when no other site can handle the supplied Host.

%% Copyright 2011 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(zotonic_status).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Zotonic Status").
-mod_descr("Default Zotonic site, used when no other site can handle the supplied Host.").
-mod_prio(10).
-mod_depends([base, bootstrap]).

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
