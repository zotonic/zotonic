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

-behaviour(gen_model).

-include_lib("zotonic.hrl").


-export([
         m_find_value/3,
         m_to_list/2,
         m_value/2,
         get/2,
         get_all/2,
         set/3,
         
         all_services_for/2,
         humanreadable/2
         ]).


%% gen_model

%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()


m_find_value(X, #m{value=undefined} = M, _Context) ->
    M#m{value=X};
m_find_value(Id, #m{value=selected}, Context) ->
    get(Id, Context);
m_find_value(Id, #m{value=humanreadable}, Context) ->
    humanreadable(Id, Context).



%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> list()
m_to_list(#m{value=undefined}, Context) ->
    [ [{value, Val}, {title, Title}] || {Val, Title} <- z_service:all(authvalues, Context)].

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
    undefined;
m_value(#m{value=_Module}, _Context) ->
    undefined.
    

%%
%% Get permissions for consumer <Id>.
%%
get(Id, Context) ->
    z_db:assoc_props("SELECT * FROM oauth_application_perm WHERE application_id = $1", [Id], Context).

%%
%% *All* applicable permissions
%%
get_all(Id, Context) ->
    [ z_service:serviceinfo(S, Context) || S <- all_services_for(Id, Context)].


insert_all([], _Id, _Context) ->
    ok;
insert_all([[] | Rest], Id, Context) ->
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
                All = [ binary_to_list(z_db:get(perm, R)) || R <- get(Id, Context)],
                lists:filter(fun(S) -> z_service:applies(All, proplists:get_value(method, S)) end, z_service:all(info, Context))
        end,
    z_depcache:memo(F, {z_services_for, Id}, ?WEEK, [z_services, {oauth_consumer, Id}], Context).

    


%%
%% Give human-readable descriptions of all services which apply;
%% collapsed with respect to "*" syntax notation.
%%
humanreadable(Id, Context) ->
    [[{desc, D}] || D <- human([string:tokens(binary_to_list(proplists:get_value(perm, R)), "/") || R <- get(Id, Context)])].


human([]) ->
    [];
human([["*"]|_Rest]) ->
    ["Access to every aspect of your account"];
human([[Module, "*"]|Rest]) ->
    Rest2 = lists:filter(fun([M,_]) -> not(M == Module) end, Rest),
    [ "Access to the " ++ z_module_manager:title(list_to_atom("mod_" ++ Module)) ++ " module" | human(Rest2)];
human([[Module, Part]|Rest]) ->
    [z_service:title(list_to_atom("service_" ++ Module ++ "_" ++ Part)) | human(Rest)].
