%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015 Marc Worrell
%% @doc Model for SQL and archive backups

%% Copyright 2015 Marc Worrell
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

-module(m_backup).

-behaviour(gen_model).

-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

m_find_value(list_backups, #m{}, Context) ->
    mod_backup:list_backups(Context);
m_find_value(is_backup_in_progress, #m{}, Context) ->
    mod_backup:backup_in_progress(Context);
m_find_value(_, #m{}, _Context) ->
    undefined.

m_to_list(#m{}, _Context) ->
    [].

m_value(#m{}, _Context) ->
    undefined.

