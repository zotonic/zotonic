%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% Date: 2009-10-04
%% @doc OAuth; API permission entry store.

%% Copyright 2009 Arjan Scherpenisse
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

-module(m_oauth_perms).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-behaviour(zotonic_model).

-include_lib("zotonic_core/include/zotonic.hrl").


-export([
    m_get/2,

     get/2,
     get_all/2,
     set/3,

     all_services_for/2,
     humanreadable/2
]).


%% gen_model

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), z:context() ) -> {term(), list()}.
m_get([], Context) ->
    All = [ [{value, Val}, {title, Title}] || {Val, Title} <- z_service:all(authvalues, Context)],
    {All, []};
m_get([ selected, Id | Rest ], Context) ->
    {get(Id, Context), Rest};
m_get([ humanreadable, Id | Rest ], Context) ->
    {humanreadable(Id, Context), Rest};
m_get(Vs, _Context) ->
    lager:error("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {undefined, []}.

%%
%% Get permissions for consumer <Id>.
%%
get(Id, Context) ->
    z_db:assoc_props("SELECT * FROM oauth_application_perm WHERE application_id = $1", [Id], Context).

%%
%% *All* applicable permissions
%%
get_all(Id, Context) ->
    all_services_for(Id, Context).


insert_all([], _Id, _Context) ->
    ok;
insert_all([[] | Rest], Id, Context) ->
    insert_all(Rest, Id, Context);
insert_all([<<>> | Rest], Id, Context) ->
    insert_all(Rest, Id, Context);
insert_all([Perm | Rest], Id, Context) ->
    z_db:q("INSERT INTO oauth_application_perm (application_id, perm) VALUES ($1, $2)", [Id, Perm], Context),
    insert_all(Rest, Id, Context).


%%
%% Set permissions for consumer <Id>.
%%
set(Id, Perms, Context) ->
    z_db:q("DELETE FROM oauth_application_perm WHERE application_id = $1", [Id], Context),
    z_depcache:flush({oauth_consumer, Id}, Context),
    insert_all(Perms, Id, Context).


%%
%% Give all services which apply for this consumer
%%
%% TODO: Refactor: Should be done via z_notifier
all_services_for(Id, Context) ->
    F = fun() ->
        All = [ proplists:get_value(perm, R) || R <- get(Id, Context)],
        lists:filter(
            fun(S) -> z_service:applies(All, proplists:get_value(method, S)) end,
            z_service:all(info, Context))
    end,
    z_depcache:memo(F, {z_services_for, Id}, ?WEEK, [z_services, {oauth_consumer, Id}], Context).




%%
%% Give human-readable descriptions of all services which apply;
%% collapsed with respect to "*" syntax notation.
%%
humanreadable(Id, Context) ->
    Split = [ binary:split(proplists:get_value(perm, R), <<"/">>) || R <- get(Id, Context) ],
    [ [{desc, D}] || D <- human(Split) ].


human([]) ->
    [];
human([ [<<"*">>] | _Rest]) ->
    [ "Access to every aspect of your account" ];
human([ [Module, <<"*">>] | Rest]) ->
    Rest2 = lists:filter(fun([M,_]) -> not(M == Module) end, Rest),
    [ "Access to the " ++ z_module_manager:title(z_service:api_prefix_to_module(Module)) ++ " module" | human(Rest2) ];
human([[Module, Part]|Rest]) ->
    [ z_service:title(binary_to_atom(<<"service_",Module/binary,"_",Part/binary>>, utf8)) | human(Rest) ].
