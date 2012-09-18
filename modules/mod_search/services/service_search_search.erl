%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% Date: 2009-12-04
%% @doc Search zotonic using the search-model

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

-module(service_search_search).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-svc_title("Search Zotonic resources.").
-svc_needauth(false).

-export([process_get/2]).

-include_lib("zotonic.hrl").

process_get(_ReqData, Context) ->
    Q1 = controller_api:get_q_all(Context),
    try
        Q = search_query:parse_request_args(Q1),
        S = z_search:search({'query', Q}, Context),
        {array, S#search_result.result}
    catch
        _: {error, {unknown_query_term, E}} ->
            {error, unknown_arg, E};
        _: {error, {Message, E}} ->
            {error, Message, E};
        _: {case_clause, {error, {error, error, _, E, _}}} ->
            {error, syntax, binary_to_list(E)}
    end.

