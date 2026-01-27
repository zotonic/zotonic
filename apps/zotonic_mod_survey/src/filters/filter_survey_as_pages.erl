%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013-2026 Marc Worrell
%% @doc Split the page blocks into pages, prepare them for easy display in the survey (form) question editor.
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

-module(filter_survey_as_pages).
-moduledoc("
Split the page blocks into pages, prepare them for easy display in the survey question editor.

A page is split in two lists: one with questions and one with the page break/options/stop
blocks.

See [mod\\_survey](/id/doc_module_mod_survey)
").

-export([
    survey_as_pages/2
]).

survey_as_pages(undefined, _Context) ->
	[];
survey_as_pages([_|_] = Blocks, _Context) ->
	split(Blocks, [], []);
survey_as_pages(_Blocks, _Context) ->
	[].

split([], [], Pages) ->
	lists:reverse(Pages);
split([], Acc, Pages) ->
	lists:reverse([{lists:reverse(Acc),[]}|Pages]);
split(Ps, Acc, Pages) ->
	case lists:splitwith(fun is_page_end/1, Ps) of
		{[], [Q|Ps1]} ->
			case maps:get(<<"name">>, Q, undefined) of
				<<"survey_feedback">> ->
					% Special block with feedback text that is
					% shown after the survey has been submitted.
					% This block is not part of the survey question
					% pages.
					split(Ps1, Acc, Pages);
				_ ->
					split(Ps1, [Q|Acc], Pages)
			end;
		{Jumps, Qs} ->
			split(Qs, [], [{lists:reverse(Acc),Jumps}|Pages])
	end.

is_page_end(P) ->
	case maps:get(<<"type">>, P, undefined) of
		<<"survey_page_break">> -> true;
		<<"survey_page_options">> -> true;
		<<"survey_stop">> -> true;
		_ -> false
	end.
