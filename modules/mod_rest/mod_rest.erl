%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell
%% @doc Makes models accessible by REST

%% Copyright 2012 Marc Worrell
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


-module(mod_rest).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("REST Interface").
-mod_description("REST API for accessing resources.").
-mod_prio(500).
-mod_provides([rest]).

-include_lib("zotonic.hrl").

%% interface functions
-export([
	observe_content_types_dispatch/3,
	observe_rsc_upload/2,
	rsc_upload/3
]).


observe_content_types_dispatch(#content_types_dispatch{}, Acc, _Context) ->
	[
		{"application/json", rest_rsc},
		{"application/x-bert", rest_rsc}
		| Acc
	].


observe_rsc_upload(#rsc_upload{id=Id, format=bert, data=Data}, Context) ->
	case catch bert:decode(Data) of
		Props when is_list(Props) ->
			rsc_upload(Id, Props, Context);
		_ ->
			{error, badarg}
	end.


rsc_upload(undefined, Props, Context) ->
	m_rsc_update:insert(update_props(Props, Context), [is_import], Context);
rsc_upload(Id, Props, Context) when is_integer(Id) ->
	m_rsc_update:update(Id, update_props(Props, Context), [is_import], Context).

update_props(Props, Context) ->
    UpdateProps = lists:filter(fun({K,_}) ->
    								is_updateable(z_convert:to_binary(K))
    						   end,
    						   Props),
    [{category, map_category(Props, Context)} | UpdateProps ].

map_category(Props, Context) ->
	case proplists:get_value(computed_category, Props) of
		undefined -> 
			other;
		List ->
			case lists:dropwhile(fun(Name) -> 
									case m_category:name_to_id(Name, Context) of
										{ok, _} -> false;
										{error, _} -> true
									end
								 end,
								 List) of
				[] -> other;
				[N|_] -> N
			end
	end.

is_updateable(<<"id">>) -> false;
is_updateable(<<"category_id">>) -> false;
is_updateable(<<"category">>) -> false;
is_updateable(<<"version">>) -> false;
is_updateable(<<"medium">>) -> false;
is_updateable(<<"created">>) -> false;
is_updateable(<<"creator_id">>) -> false;
is_updateable(<<"modified">>) -> false;
is_updateable(<<"modifier_id">>) -> false;
is_updateable(<<"page_url">>) -> false;
is_updateable(<<"props">>) -> false;
is_updateable(<<"computed_", _/binary>>) -> false;
is_updateable(<<"pivot_", _/binary>>) -> false;
is_updateable(_) -> true.

