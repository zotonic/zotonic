%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% @doc Select the language from a list of languages

%% Copyright 2011 Marc Worrell
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

-module(filter_language).
-export([
    language/2
]).

-include("zotonic.hrl").

language(undefined, Context) ->
    z_context:language(Context);
language(B, Context) when is_binary(B) ->
    z_context:language(Context);
language(Id, Context) when is_integer(Id) ->
    language(m_rsc:p_no_acl(Id, language, Context), Context);
language(ISO, _Context) when is_atom(ISO) ->
    ISO;
language({trans, Tr}, Context) ->
    language([ L || {L,_} <- Tr ], Context);
language([], Context) ->
    z_context:language(Context);
language(Langs, Context) ->
    z_trans:lookup_fallback_language(Langs, z_context:language(Context), Context).
