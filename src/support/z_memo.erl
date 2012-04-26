%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-11-05
%%
%% @doc Simple memo functions.  Stores much used values in the process dictionary. Especially useful for
%% ACL lookups.

%% Copyright 2009 Marc Worrell
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

-module(z_memo).
-author("Marc Worrell <marc@worrell.nl>").

-export([
	enable/0,
	disable/0,
	is_enabled/1,
	set_userid/1,
	set/2,
	set/3,
	get/1,
	get/2,
	delete/1
]).

-include_lib("include/zotonic.hrl").

%% @doc Enable memoization for this process. You need to call set_userid/1 before memoization is effective.
enable() ->
	erlang:put(is_memo, true).

%% @doc Disable memoization for this process, also cleans up the possible depcache memoization.
disable() ->
    z_depcache:flush_process_dict(),
	erlang:erase(is_memo),
	erlang:erase(memo_userid).

%% @doc Set the user id for which we memo values.  Called by z_auth on session initialization.
set_userid(AuthUserId) ->
	case erlang:get(is_memo) of
		true -> erlang:put(memo_userid, {ok, AuthUserId});
		_ -> error
	end.

%% @doc Check if memoization is enabled for the current user/process.  Disabled when in a sudo action.
is_enabled(#context{acl=admin}) ->
	false;
is_enabled(#context{user_id=UserId}) ->
	case erlang:get(memo_userid) of
		{ok, UserId} -> true;
		_ -> false
	end.

%% @doc Check if the key is stored.
get(Key) ->
	erlang:get(Key).

get(Key, Context) ->
	case is_enabled(Context) of
		true -> erlang:get(Key);
		false -> undefined
	end.

%% @doc Store a key if memoization is set.
set(Key, Value) ->
	erlang:put(Key, Value),
	Value.

set(Key, Value, Context) ->
	case is_enabled(Context) of
		true -> erlang:put(Key, Value);
		false -> nop
	end,
	Value.

delete(Key) ->
    erlang:erase(Key).


