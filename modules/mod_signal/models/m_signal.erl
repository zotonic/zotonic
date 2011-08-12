% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2010 Maas-Maarten Zeeman
%% Date: 2010-12-03
%% @doc Connect a page to a signal

%% Copyright 2010 Maas-Maarten Zeeman
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

-module(m_signal).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-behaviour(gen_model).

-export([m_find_value/3,
	 m_to_list/2,
	 m_value/2]).

-include("zotonic.hrl").

%
%
m_find_value({SignalType, SignalProps}, #m{value=undefined}=M, _Context) when is_atom(SignalType) ->
    M#m{value={SignalType, SignalProps}};
m_find_value(Name, #m{value={_SignalType, SignalProps}}, _Context) ->
    proplists:get_value(Name, SignalProps);

m_find_value(type, #m{value=undefined}=M, _Context) ->
    M#m{value=type};
m_find_value(props, #m{value=undefined}=M, _Context) ->
    M#m{value=props};
m_find_value({SignalType, _SignalProps}, #m{value=type}, _Context) ->
    SignalType;
m_find_value({_SignalType, SignalProps}, #m{value=props}, _Context) ->
    SignalProps.

%
m_to_list(_Value, _Context) ->
    [].

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(_Value, _Context) ->
    undefined.
