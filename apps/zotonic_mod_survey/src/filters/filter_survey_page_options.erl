%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Fetch all page options from a list of blocks.
%% @end

%% Copyright 2013-2026 Marc Worrell
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

-module(filter_survey_page_options).
-moduledoc("
Check the list of blocks and collect all options for that page.

Options are:

 * `is_stop_page`: stop survey, no submit button
 * `is_hide_back`: hide back button

The options could be in two types of blocks: the `survey_stop` and the `survey_page_options` blocks.
The `survey_stop` block is deprecated in favor of the `survey_page_options` block.

When displaying or editing a survey, the `survey_page_options` block is collected together with all
page jump blocks.

See [mod\\_survey](/id/doc_module_mod_survey)
").

-export([
    survey_page_options/2
]).

survey_page_options(undefined, _Context) ->
    #{};
survey_page_options(Qs, _Context) ->
    lists:foldl(
        fun option_block/2,
        #{},
        Qs).

option_block(#{ <<"type">> := <<"survey_stop">> }, Acc) ->
    Acc#{
        <<"is_stop_page">> => true
    };
option_block(#{ <<"type">> := <<"survey_page_options">> } = Opts, Acc) ->
    Acc#{
        <<"is_hide_back">> => maps:get(<<"is_hide_back">>, Opts, maps:get(<<"is_hide_back">>, Acc, false)),
        <<"is_stop_page">> => maps:get(<<"is_stop_page">>, Opts, maps:get(<<"is_stop_page">>, Acc, false))
    };
option_block(_Block, Acc) ->
    Acc.
