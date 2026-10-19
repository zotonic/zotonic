%% @copyright 2026 Marc Worrell
%% @doc Dispatch prepared resource imports to observers or their default importer.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(z_import_resource).

-export([import/3]).
-export_type([item/0, result/0, observer_result/0]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(ACTIVE_IMPORTS, import_resource_active_uris).

-type item() :: #{
    uri := binary(),
    props := map(),
    edges => [{term(), term()}],
    medium => binary() | map() | undefined,
    atom() => term()
}.
-type result() :: {ok, m_rsc:resource_id(), m_rsc_import:import_map()}
    | {skip, term()} | {error, term()}.
-type observer_result() :: undefined | {ok, m_rsc:resource_id()} | result().

%% @doc Offer a prepared item to observers, then invoke its default importer.
%% Does not open a transaction or elevate permissions. The caller owns tracking,
%% completion notifications and the interpretation of skip/error results.
%% Delegation for the same URI does not notify twice; other URIs still notify.
-spec import(Notification, Default, Context) -> Result when
    Notification :: #import_resource{},
    Default :: fun((z:context()) -> result()),
    Context :: z:context(),
    Result :: result().
import(#import_resource{item = #{uri := Uri}} = Notification, Default, Context) ->
    Active = case z_context:get(?ACTIVE_IMPORTS, Context) of
        undefined -> [];
        Uris -> Uris
    end,
    case lists:member(Uri, Active) of
        true -> Default(Context);
        false ->
            ImportContext = z_context:set(?ACTIVE_IMPORTS, [Uri | Active], Context),
            case z_notifier:first(Notification, ImportContext) of
                undefined -> Default(ImportContext);
                Result -> observer_result(Result, ImportContext)
            end
    end.

observer_result({ok, Id}, Context) ->
    observer_result({ok, Id, #{}}, Context);
observer_result({ok, Id, Imported}, Context) when is_integer(Id), is_map(Imported) ->
    case lists:all(fun({Uri, ResourceId}) -> is_binary(Uri) andalso is_integer(ResourceId) end,
            maps:to_list(Imported)) of
        false -> {error, invalid_observer_result};
        true ->
            case z_acl:rsc_editable(Id, Context) andalso
                lists:all(fun(ResourceId) -> z_acl:rsc_visible(ResourceId, Context) end,
                    maps:values(Imported))
            of
                true -> {ok, Id, Imported};
                false -> {error, eacces}
            end
    end;
observer_result({skip, _} = Skip, _) -> Skip;
observer_result({error, _} = Error, _) -> Error;
observer_result(_, _) -> {error, invalid_observer_result}.
