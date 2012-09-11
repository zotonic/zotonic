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
			rsc_upload(proplists:get_value(id, Props), Props, Context);
		_ ->
			{error, badarg}
	end.


rsc_upload(undefined, Props, Context) ->
    UpdateProps = lists:filter(fun is_updateable/1, Props),
	m_rsc_update:insert(UpdateProps, [is_import], Context);
rsc_upload(Id, Props, Context) when is_integer(Id) ->
    UpdateProps = lists:filter(fun is_updateable/1, Props),
	Site = z_context:site(Context),
	case proplists:lookup(computed_site, Props) of
		{site, Site} ->
			% Import into our own site, keep the id
			m_rsc_update:update(Id, Props, [is_import], Context);
		_ ->
			% Import of an id from another site
			case m_rsc:exists(Id, Context) of
				true ->
					m_rsc_update:insert(UpdateProps, [is_import], Context);
				false ->
					m_rsc_update:update(Id, Props, [is_import], Context)
			end
	end.


is_updateable({<<"id">>, _}) -> false;
is_updateable({<<"version">>, _}) -> false;
is_updateable({<<"medium">>, _}) -> false;
is_updateable({<<"created">>, _}) -> false;
is_updateable({<<"creator_id">>, _}) -> false;
is_updateable({<<"modified">>, _}) -> false;
is_updateable({<<"modifier_id">>, _}) -> false;
is_updateable({<<"page_url">>, _}) -> false;
is_updateable({<<"props">>, _}) -> false;
is_updateable({<<"computed_", _/binary>>, _}) -> false;
is_updateable({<<"pivot_", _/binary>>, _}) -> false;
is_updateable(_) -> true.

