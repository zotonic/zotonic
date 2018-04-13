%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-11-20
%%
%% @doc Model for accessing the session variables from a template.

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

-module(m_session).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(gen_model).

%% interface functions
-export([
    m_get/2
]).

-include_lib("zotonic.hrl").


%% @doc Fetch the value for the key from a model source
-spec m_get( list(), z:context() ) -> {term(), list()}.
m_get(_, _Context) ->
    {undefined, []}.
% m_get([ session_id | Rest ], Context) ->
%     {Context#context.session_id, Rest};
% m_get([ page_id | Rest ], Context) ->
%     {Context#context.page_id, Rest};
% m_get([ Key | Rest ], Context) ->
%     {z_context:get_session(Key, Context), Rest};
% m_get(Vs, _Context) ->
%     lager:error("Unknown ~p lookup: ~p", [?MODULE, Vs]),
%     {undefined, []}.
