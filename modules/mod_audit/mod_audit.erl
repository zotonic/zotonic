%% Copyright 2019 Zotonic
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
-module(mod_audit).
-author("David de Boer <david@ddeboer.nl>").

-mod_title("Audit log").
-mod_description("Audit log of content and admin changes").
-mod_depends([mod_logging]).
-mod_prio(400).

-export([
    observe_m_config_update/2,
    observe_module_activate/2,
    observe_module_deactivate/2,
    observe_auth_checked/2,
    observe_auth_logoff/3,
    observe_edge_insert/2,
    observe_edge_delete/2,
    observe_media_update_done/2,
    observe_rsc_update_done/2
]).

-include_lib("zotonic.hrl").

observe_auth_checked(#auth_checked{id = Id, is_accepted = true, username = Username}, Context) ->
    log(
        #log_audit{
            action = auth_success,
            rsc_id = Id,
            value = Username
        },
        Context
    );
observe_auth_checked(#auth_checked{id = Id, is_accepted = false, username = Username}, Context) ->
    log(
        #log_audit{
            action = auth_failed,
            rsc_id = Id,
            value = Username
        },
        Context
    ).

observe_auth_logoff(auth_logoff, AccContext, Context) ->
    log(
        #log_audit{
            action = auth_logoff
        },
        Context
    ),
    AccContext.

observe_media_update_done(#media_update_done{action = Action, post_props = Props}, Context) ->
    log({media, Action, Props}, Context).

observe_module_activate(#module_activate{module = Module}, Context) ->
    log(
        #log_audit{
            action = module_activate,
            value = Module
        },
        Context
    ).

observe_module_deactivate(#module_deactivate{module = Module}, Context) ->
    log(
        #log_audit{
            action = module_deactivate,
            value = Module
        },
        Context
    ).

observe_edge_insert(#edge_insert{subject_id = Subject, predicate = Predicate, object_id = Object}, Context) ->
    log(
        #log_audit{
            action = edge_insert,
            rsc_id = Subject,
            value = {Predicate, Object}
        },
        Context
    ).

observe_edge_delete(#edge_delete{subject_id = Subject, predicate = Predicate, object_id = Object}, Context) ->
    log(
        #log_audit{
            action = edge_delete,
            rsc_id = Subject,
            previous_value = {Predicate, Object}
        },
        Context
    ).

observe_rsc_update_done(#rsc_update_done{id = Id, pre_props = Pre, post_props = Post}, Context) ->
    log(
        #log_audit{
            action = rsc_update_done,
            rsc_id = Id,
            value = Post,
            previous_value = Pre
        },
        Context
    ).

observe_m_config_update(#m_config_update{module = Module, key = Key, value = Value}, Context) ->
    log(
        #log_audit{
            action = config_update,
            previous_value = m_config:get(Module, Key),
            value = Value
        },
        Context
    ).

-spec log(#log_audit{}, z:context()) -> ok.
log(Entry, Context) ->
    z_notifier:notify(
        #zlog{
            type = audit,
            user_id = z_acl:user(Context),
            timestamp = os:timestamp(),
            props = Entry
        },
        Context
    ).
