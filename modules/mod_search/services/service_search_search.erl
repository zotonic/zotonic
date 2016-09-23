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

-export([process_get/1]).

-define(MAX_LIMIT, 1000).

-include_lib("zotonic.hrl").

process_get(Context) ->
    try
        Format = z_context:get_q(<<"format">>, Context, <<"ids">>),
        Limit = erlang:min(?MAX_LIMIT, z_convert:to_integer(z_context:get_q(<<"limit">>, Context, <<"20">>))),
        Offset = 1+erlang:max(0, z_convert:to_integer(z_context:get_q(<<"offset">>, Context, <<"0">>))),
        Q1 = lists:foldl(fun proplists:delete/2, controller_api:get_q_all(Context), [<<"offset">>, <<"limit">>, <<"format">>]),
        Q = search_query:parse_request_args(Q1),
        S = z_search:search({'query', Q}, {Offset, Limit}, Context),
        convert_result(Format, S#search_result.result, Context)
    catch
        _: {error, {unknown_query_term, E}} ->
            {error, unknown_arg, E};
        _: {error, {Message, E}} ->
            {error, Message, E};
        _: {case_clause, {error, {error, error, _, E, _}}} ->
            {error, syntax, binary_to_list(E)}
    end.


convert_result(<<"ids">>, Ids, _Context) ->
    {array, Ids};

convert_result(<<"simple">>, Ids, Context) ->
    {array, [format_simple(Id, Context) || Id <- Ids]};

convert_result(F, _, _) ->
    {error, unknown_arg, "format=" ++ F}.


format_simple(Id, Context) ->
    Preview = case z_media_tag:url(Id, [{width, 800}, {height, 800}, {upscale, true}, {use_absolute_url, true}], Context) of
                  {ok, P} -> [{preview_url, P}];
                  _ -> []
              end,
    z_convert:to_json(
      [{id, Id},
       {title, m_rsc:p(Id, title, Context)},
       {category, m_rsc:is_a(Id, Context)},
       {summary, m_rsc:p(Id, summary, Context)} | Preview
      ]
     ).
