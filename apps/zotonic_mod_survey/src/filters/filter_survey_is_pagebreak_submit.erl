%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2025 Marc Worrell
%% @doc Check if the page breaks at the end of this page are submitting
%% without any conditions.
%% @end

%% Copyright 2025 Marc Worrell
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

-module(filter_survey_is_pagebreak_submit).
-moduledoc("
Check if a list of questions contains a pagebreak block with an unconditional submit.
").

-export([
    survey_is_pagebreak_submit/2
]).

survey_is_pagebreak_submit(Qs, _Context) ->
    find_submit_jump(Qs).

find_submit_jump([]) ->
    false;
find_submit_jump([ #{<<"type">> := <<"survey_page_break">>, <<"target1">> := <<"submit">> } = Q | _Qs ]) ->
    maps:get(<<"condition1">>, Q, <<>>) =:= <<>>;
find_submit_jump([#{ <<"type">> := <<"survey_page_break">>, <<"target2">> := <<"submit">> } = Q | _Qs ]) ->
    maps:get(<<"condition2">>, Q, <<>>) =:= <<>>;
find_submit_jump([_Q|Qs]) ->
    find_submit_jump(Qs).
