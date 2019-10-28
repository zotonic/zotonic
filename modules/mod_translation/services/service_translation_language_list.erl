%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Marc Worrell
%% @doc List all enabled languages.

%% Copyright 2019 Marc Worrell
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

-module(service_translation_language_list).
-author("Marc Worrell <marc@worrell.nl>").

-svc_title("List all enabled languages.").
-svc_needauth(false).

-export([process_get/2]).

-include_lib("zotonic.hrl").

process_get(_ReqData, Context) ->
    List = m_translation:language_list_enabled(Context),
    List1 = lists:sort(
        fun({_, AProps}, {_, BProps}) ->
            lists:keyfind(language, 1, AProps) =< lists:keyfind(language, 1, BProps)
        end,
        List),
    iolist_to_binary(
        mochijson:binary_encode(
            lists:map(
                fun({Code, Props}) ->
                    [ {iso, Code} | Props ]
                end,
                List1))).
