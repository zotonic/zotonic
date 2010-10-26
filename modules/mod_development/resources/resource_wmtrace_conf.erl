%% @author Atilla Erdodi <atilla@maximonster.com>
%% @copyright 2010 Maximonster Interactive Things
%% @date 2010-10-20
%% @doc Resource to configure tracing of webmachine requests.

%% Copyright 2010 Maximonster Interactive Things
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
%%
%% Resource
%%

-module(resource_wmtrace_conf).

-export([
    event/2
        ]).


-include_lib("resource_html.hrl").
-include_lib("webmachine_logger.hrl").

%% GLOBAL
%% only 5xx
%% 4xx and 5xx

%% PER RESOURCE
%% always


event({submit, {add, _Args}, _TriggerId, _TargetId}, Context) ->
    Res  = z_context:get_q("resource", Context),
    case ets:lookup(?WMTRACE_CONF_TBL, Res) of
        [] ->
            Eagerness = immediate,
            ets:insert(?WMTRACE_CONF_TBL, {list_to_atom(Res), Eagerness}),
            ok;
        _ ->
            already_added
    end,
    z_render:wire({reload, []}, Context);
event({postback, {set_global, _Args}, _TriggerId, _TargetId}, Context) ->
    Policy = list_to_atom(z_context:get_q("triggervalue", Context)),
    case Policy of
        disabled ->
            ets:delete(?WMTRACE_CONF_TBL, trace_global);
        Policy_ ->
            ets:delete(?WMTRACE_CONF_TBL, trace_global),
            ets:insert(?WMTRACE_CONF_TBL, {trace_global, Policy_})
    end,
    z_render:growl("Changed global resource tracing setting.", Context);
event({postback, {edit, _Args}, _TriggerId, _TargetId}, Context) ->
    %% not used yet, since it is only meaningful if we need 
    %% more trace options for per-resource tracing
    % Res  = proplists:get_value(resource, Args),
    % NewOpts = {},
    % ets:delete(?WMTRACE_CONF_TBL, Res),
    % ets:insert(?WMTRACE_CONF_TBL, {Res, NewOpts}),    
    Context;
event({postback, {delete, Args}, _TriggerId, _TargetId}, Context) ->    
    Res  = proplists:get_value(res_to_del, Args),
    ets:delete(?WMTRACE_CONF_TBL, Res),
    z_render:wire({reload, []}, Context).


html(Context) ->
    Resources_ = get_resources(Context),
    TraceConf_ = get_trace_conf(?WMTRACE_CONF_TBL, ets:first(?WMTRACE_CONF_TBL), []),
    TraceConf = [Res || {Res, _Eagerness} <- TraceConf_],
    Resources = Resources_ -- TraceConf,
    
    TraceGlobal = case ets:lookup(?WMTRACE_CONF_TBL, trace_global) of
                      [] -> "disabled";
                      [{trace_global, TrGl}] -> atom_to_list(TrGl)
                  end,
    Vars = [{trace_global, TraceGlobal}, {trace_conf, TraceConf}, 
            {res, Resources}, {page_admin_wmtrace_conf, true}],
    Html = z_template:render("wmtrace_conf.tpl", Vars, Context),
    z_context:output(Html, Context).

get_trace_conf(Tbl, trace_dir, Acc) ->
    get_trace_conf(Tbl, ets:next(Tbl, trace_dir), Acc);
get_trace_conf(Tbl, trace_global, Acc) ->
    get_trace_conf(Tbl, ets:next(Tbl, trace_global), Acc);
get_trace_conf(_Tbl, '$end_of_table', Acc) ->    
    Acc;
get_trace_conf(Tbl, Key, Acc) ->
    [Res] = ets:lookup(Tbl, Key),
    get_trace_conf(Tbl, ets:next(Tbl, Key), [Res | Acc]).

get_resources(Context) ->
    {_, _, _, _, _, SiteDispatch} = z_dispatcher:dispatchinfo(Context),
    Resources = lists:foldl(fun({_, _, Resource, _}, ResList) -> 
                                    case lists:member(Resource, ResList) of
                                        false -> [Resource | ResList];
                                        true -> ResList
                                    end
                            end, [], SiteDispatch),
    lists:reverse(Resources).

