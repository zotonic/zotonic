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

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/2,

    get/2,
    get_new_location/2,
    is_gone/2,
    gone/2,
    gone/3
]).

-include_lib("zotonic.hrl").


%% @doc Fetch the value for the key from a model source
-spec m_get( list(), z:context() ) -> {term(), list()}.
m_get([ Id, new_location | Rest ], Context) when is_integer(Id) ->
    {get_new_location(Id, Context), Rest};
m_get([ Id, is_gone | Rest ], Context) when is_integer(Id) ->
    {is_gone(Id, Context), Rest};
m_get([ Id | Rest ], Context) when is_integer(Id) ->
    {get(Id, Context), Rest};
m_get(Vs, _Context) ->
    lager:error("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {undefined, []}.

%% @doc Get the possible 'rsc_gone' resource for the id.
get(Id, Context) when is_integer(Id) ->
    F = fun() ->
        z_db:assoc_row("select * from rsc_gone where id = $1", [Id], Context)
    end,
    z_depcache:memo(F, {rsc_gone, Id}, Context).


%% @doc Get the redirect location for the id, uses the current dispatch rule and otherwise
%%      the 'id' dispatch rule.
get_new_location(undefined, _Context) ->
    undefined;
get_new_location(Id, Context) when is_integer(Id) ->
    F = fun() ->
            z_db:q_row("select new_id, new_uri from rsc_gone where id = $1 limit 1", [Id], Context)
        end,
    case z_depcache:memo(F, {rsc_gone_new_location, Id}, Context) of
        undefined ->
            undefined;
        {undefined, undefined} ->
            undefined;
        {NewId, _} when is_integer(NewId) ->
            NewUri = case z_context:get(zotonic_dispatch, Context) of
                         undefined ->
                             z_dispatcher:url_for(id, [{id, NewId}], Context);
                         Dispatch ->
                             case z_dispatcher:url_for(Dispatch, [{id, NewId}], Context) of
                                 undefined -> z_dispatcher:url_for(id, [{id, NewId}], Context);
                                 DispUri -> DispUri
                             end
                     end,
            z_context:abs_url(NewUri, Context);
        {undefined, NewUri} ->
            z_context:abs_url(NewUri, Context)
    end.


%% @doc Check if the resource used to exist.
-spec is_gone(integer()|undefined, #context{}) -> boolean().
is_gone(undefined, _Context) ->
    false;
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
    case z_db:assoc_row("
            select id, name, version, page_path, uri, is_authoritative, creator_id, created
            from rsc
            where id = $1
            ", [Id], Context)
    of
        undefined ->
            {error, notfound};
        Props when is_list(Props) ->
            Result = z_db:transaction(
                    fun(Ctx) ->
                        Props1 = [
                            {new_id, NewId},
                            {new_uri, undefined},
                            {modifier_id, z_acl:user(Ctx)}
                            | Props
                        ],
                        case z_db:q1("select count(*) from rsc_gone where id = $1", [Id], Ctx) of
                            1 ->
                                lager:warning(z_context:lager_md(Ctx),
                                              "[~p] Second rsc_gone entry for id ~p",
                                              [z_context:site(Ctx), Id]),
                                {ok, _} = z_db:update(rsc_gone, Id, Props1, Ctx),
                                {ok, Id};
                            0 ->
                                z_db:insert(rsc_gone, Props1, Ctx)
                        end
                    end,
                    Context),
            case Result of
                {error, {error, error, <<"23505">>, _ErrMsg, _ErrProps}} ->
                    % Duplicate key - ignore (race condition)
                    {ok, Id};
                Other -> Other
            end
    end.
