%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell
%%
%% @doc Model for administration of deleted resources and their possible new location.

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

-module(m_rsc_gone).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,
    
    get/2,
    is_gone/2,
    gone/2,
    gone/3
]).

-include_lib("zotonic.hrl").

%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(_Key, #m{value=undefined}, _Context) ->
	undefined.

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> List
m_to_list(#m{value=undefined}, _Context) ->
    [].

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
	undefined.



%% @doc Get the possible 'rsc_gone' resource for the id.
get(Id, Context) when is_integer(Id) ->
	F = fun() ->
			z_db:assoc_row("select * from rsc_gone where id = $1", [Id], Context) 
		end,
	z_depcache:memo(F, {rsc_gone, Id}, Context).


%% @doc Check if the resource used to exist.
-spec is_gone(integer(), #context{}) -> boolean().
is_gone(Id, Context) when is_integer(Id) ->
	F = fun() ->
			z_db:q1("select count(*) from rsc_gone where id = $1", [Id], Context) =:= 1
		end,
	z_depcache:memo(F, {rsc_is_gone, Id}, Context).

%% @doc Copy a resource to the 'gone' table, use the current user as the modifier (deleter).
-spec gone(integer(), #context{}) -> {ok, integer()}.
gone(Id, Context) when is_integer(Id) ->
	gone(Id, undefined, Context).

%% @doc Copy a resource to the 'gone' table, use the current user as the modifier (deleter).
%%      Also sets the 'new id', which is the id that replaces the deleted id.
gone(Id, NewId, Context) when is_integer(Id), is_integer(NewId) orelse NewId =:= undefined ->
	Props = z_db:assoc_row("
			select id, name, version, page_path, uri, is_authoritative, creator_id, created
			from rsc
			where id = $1
		", [Id], Context),
	Props1 = [
		{new_id, NewId},
		{new_uri, undefined},
		{modifier_id, z_acl:user(Context)}
		| Props 
	],
	case z_db:q1("select count(*) from rsc_gone where id = $1", [Id], Context) of
		1 ->
			lager:error(z_context:lager_md(Context), "Second rsc_gone entry for id ~p", [Id]), 
			z_db:update(rsc_gone, Id, Props1, Context);
		0 ->
			z_db:insert(rsc_gone, Props1, Context)
	end.

