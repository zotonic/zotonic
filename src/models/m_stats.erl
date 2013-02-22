%% @author Andreas Stenius <git@astekk.se>
%% @copyright 2013 Andreas Stenius
%% Date: 2013-02-22
%%
%% @doc Model for the zotonic site configuration

%% Copyright 2013 Andreas Stenius
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

-module(m_stats).
-author("Andreas Stenius <git@astekk.se").

-behaviour(gen_model).

%% interface functions
-export([
         m_find_value/3,
         m_to_list/2,
         m_value/2
        ]).

-include_lib("zotonic.hrl").

%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(metrics, #m{ value=undefined }, Context) ->
    get_metrics(Context);
m_find_value(metric, #m{ value=undefined }=M, _Context) ->
    M#m{ value=metric };
m_find_value(Key, #m{ value=undefined }=M, _Context) ->
    M#m{ value={key, Key} };
m_find_value(System, #m{ value=metric }=M, _Context) ->
    M#m{ value={metric, System} };
m_find_value(Name, #m{ value={metric, System} }=M, #context{ host=Host }) ->
    M#m{ value={key, {Host, System, Name}} };
m_find_value(system, #m{ value={key, {_, System, _}} }, _Context) ->
    System;
m_find_value(name, #m{ value={key, {_, _, Name}} }, _Context) ->
    Name;
m_find_value(type, #m{ value={key, Key} }, _Context) ->
    get_metric_type(Key);
m_find_value(value, #m{ value={key, Key} }, _Context) ->
    get_metric_value(Key, get_metric_type(Key)).

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> All
m_to_list(#m{ value=undefined }, Context) ->
    get_metrics(Context).

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{ value=undefined }, _Context) ->
    undefined.


get_metrics(#context{ host=Host }) ->
    M = #m{ model=?MODULE },
    [M#m{ value={key, Key} } || {H,_,_}=Key <- folsom_metrics:get_metrics(), H==Host].

get_metric_type(Key) ->
    [{Key, [{type, Type}]}] = folsom_metrics:get_metric_info(Key),
    Type.

get_metric_value(Key, histogram) ->
    folsom_metrics:get_histogram_statistics(Key);
get_metric_value(Key, _) ->
    folsom_metrics:get_metric_value(Key).
