%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-07-02
%% @doc Support for editing predicates in the admin module.  Also hooks into the rsc update function to
%% save the specific fields for predicates

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

-module(mod_admin_predicate).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Admin predicate support").
-mod_description("Adds support for editing predicates to the admin.").
-mod_prio(600).

%% interface functions
-export([
    observe_rsc_update/3,
    observe_rsc_update_done/2
]).

-include_lib("zotonic.hrl").

%% @doc Check if the update contains information for a predicate.  If so then update
%% the predicate information in the db and remove it from the update props.
%% @spec observe_rsc_update({rsc_update, ResourceId, OldResourceProps}, {Changed, UpdateProps}, Context) -> {NewChanged, NewUpdateProps}
observe_rsc_update(#rsc_update{id=Id}, {Changed, Props}, Context) ->
    case       proplists:is_defined(predicate_subject, Props) 
        orelse proplists:is_defined(predicate_object, Props) of

        true ->
            Subjects = proplists:get_all_values(predicate_subject, Props),
            Objects  = proplists:get_all_values(predicate_object, Props),
            m_predicate:update_noflush(Id, Subjects, Objects, Context),

            Props1 = proplists:delete(predicate_subject, 
                        proplists:delete(predicate_object, Props)),
            {true, Props1};
        false ->
            {Changed, Props}
    end.

%% @doc Whenever a predicate has been updated we have to flush the predicate cache.
observe_rsc_update_done(#rsc_update_done{pre_is_a=BeforeCatList, post_is_a=CatList}, Context) ->
    case lists:member(predicate, CatList) orelse lists:member(predicate, BeforeCatList) of
        true -> m_predicate:flush(Context);
        false -> ok
    end.
