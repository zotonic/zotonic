%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2026 Marc Worrell
%% @doc Simple memo functions.  Stores much used values in the process dictionary. Especially useful for
%% ACL lookups or often used functions. This is enabled for all HTTP and MQTT requests.
%% @end

%% Copyright 2009-2026 Marc Worrell
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
	enable/1,
	disable/0,
	flush/1,
	is_enabled/1,
	set_userid/1,
	set/2,
	set/3,
	get/1,
	get/2,
	delete/1,
	get_status/0,
	set_status/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Enable memoization for this process. You need to call set_userid/1 before memoization is effective.
enable() ->
	z_depcache:in_process(true),
	erlang:put(is_memo, true).

%% @doc Enable memoization for this process, calls set_userid/1 for the given context.
enable(Context) ->
	enable(),
	set_userid(Context).

%% @doc Disable memoization for this process, also cleans up the possible depcache memoization.
disable() ->
    z_depcache:flush_process_dict(),
	erlang:erase(is_memo),
	erlang:erase(memo_userid).

%% @doc Flush the memo cache and set the cache to the current user - needed when changing users
flush(Context) ->
	flush(),
	case erlang:get(is_memo) of
		true -> set_userid(Context);
		_ -> ok
	end.

%% @doc Flush all memo keys from the process dict.
flush() ->
	z_depcache:flush_process_dict(),
	[ erlang:erase(Key) || {memo, _} = Key <- erlang:get_keys() ],
	erlang:erase(memo_userid),
	ok.

%% @doc If a new user id is set in the context, and the user is different from the previous user,
%% then reset the memoization cache and start with the new user id.
set_userid(#context{ user_id = NewUserId, site = Site }) ->
	case erlang:get(is_memo) of
		true ->
			case erlang:get(memo_userid) of
				{ok, Site, NewUserId} ->
					ok; % same user, do nothing
				_ ->
					flush(),
					erlang:put(memo_userid, {ok, Site, NewUserId})
			end;
		_ ->
			ok
	end.

%% @doc Check if memoization is enabled for the current user/process.  Disabled when in a sudo action.
is_enabled(#context{ acl = admin }) ->
	false;
is_enabled(#context{ user_id = UserId, site = Site }) ->
	case erlang:get(memo_userid) of
		{ok, Site, UserId} -> true;
		_ -> false
	end.

%% @doc Check if the key is stored. Irrespective of current user-id or site context.
get(Key) ->
	erlang:get({memo, Key}).

%% @doc Check if the cache is enabled for the user and if the key is stored.
get(Key, Context) ->
	case is_enabled(Context) of
		true -> erlang:get({memo, Key});
		false -> undefined
	end.

%% @doc Store a key. Irrespective of current user-id or site context.
set(Key, Value) ->
	erlang:put({memo, Key}, Value),
	Value.

%% @doc Store the value if enabled for the current user.
set(Key, Value, Context) ->
	case is_enabled(Context) of
		true ->
			erlang:put({memo, Key}, Value);
		false -> nop
	end,
	Value.

%% @doc Delete a key from the cache. Return the previous value and 'undefined' if not set.
delete(Key) ->
    erlang:erase({memo, Key}).

%% @doc Fetch the current memo settings, used when copying the memo settings to a new process.
get_status() ->
	{erlang:get(is_memo), erlang:get(memo_userid)}.

%% @doc Set the memo settings from a previous get_status/0. Used when copying the memo
%% settings to a new process.
set_status({true, MemoUserId}) ->
	enable(),
	case erlang:get(memo_userid) of
		MemoUserId ->
			ok;
		_ ->
			flush(),
			erlang:put(memo_userid, MemoUserId)
	end;
set_status({_False, _MemoUserId}) ->
	disable().
