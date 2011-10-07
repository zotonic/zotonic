%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% Date: 2010-05-03
%% @doc Simple ACL module. Any user gets full admin privileges.  Useful for a simple site or blog.

%% Copyright 2010 Marc Worrell
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

-module(mod_acl_adminonly).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("ACL Admins Only").
-mod_description("Simple access control module, all users are site administrators. Use this for a simple site.").
-mod_prio(500).

%% interface functions
-export([
    observe_acl_is_allowed/2,
    observe_acl_can_see/2,
    observe_acl_logon/2,
    observe_acl_logoff/2,
    observe_acl_rsc_update_check/3
]).

-include("zotonic.hrl").

%% @doc Check if the user is allowed to perform Action on Object
observe_acl_is_allowed(#acl_is_allowed{action=view, object=Id}, #context{user_id=undefined} = Context) when is_integer(Id) ->
	Acl = m_rsc:get_acl_props(Id, Context),
    case Acl#acl_props.is_published of
        false -> 
            false;
        true ->
            case Acl#acl_props.visible_for == 0 of
                false ->
                    false;
                true ->
                    Date = calendar:local_time(),
                    Acl#acl_props.publication_start =< Date andalso Acl#acl_props.publication_end >= Date
            end
    end;	
observe_acl_is_allowed(#acl_is_allowed{}, #context{user_id=undefined}) ->
	false;
observe_acl_is_allowed(#acl_is_allowed{action=update, object=Id}, Context) when is_integer(Id) ->
	case m_rsc:p(Id, is_authoritative, Context) of
		true -> true;
		_ -> undefined
	end;
observe_acl_is_allowed(#acl_is_allowed{}, _Context) ->
	true.

%% @doc Return the max visible_for an user can see, used for pruning during searches
observe_acl_can_see(#acl_can_see{}, #context{user_id=undefined}) ->
	?ACL_VIS_PUBLIC;
observe_acl_can_see(#acl_can_see{}, _Context) ->
	?ACL_VIS_USER.
	
%% @doc Let the user log on, this is the moment to start caching information.
observe_acl_logon(#acl_logon{id=UserId}, Context) ->
	Context#context{acl=?MODULE, user_id=UserId}.

%% @doc Let the user log off, clean up any cached information.
observe_acl_logoff(#acl_logoff{}, Context) ->
	Context#context{acl=undefined, user_id=undefined}.

%% @doc Filter the properties before an update. Return filtered/updated resource proplist or
%% the tuple {error, Reason}
observe_acl_rsc_update_check(#acl_rsc_update_check{}, {error, Reason}, _Context) ->
	{error, Reason};
observe_acl_rsc_update_check(#acl_rsc_update_check{id=insert_rsc}, Props, _Context) ->
	PropsPubl = case proplists:get_value(is_published, Props) of
		undefined -> z_utils:prop_replace(is_published, false, Props);
		_ -> Props
	end,
	PropsVis = case proplists:get_value(visible_for, PropsPubl) of
		undefined -> z_utils:prop_replace(visible_for, ?ACL_VIS_PUBLIC, PropsPubl);
		_ -> PropsPubl
	end,
	case proplists:get_value(is_authoritative, PropsVis) of
		undefined -> z_utils:prop_replace(is_authoritative, true, PropsVis);
		_ -> PropsVis
	end;
observe_acl_rsc_update_check(#acl_rsc_update_check{}, Props, _Context) ->
	Props.


