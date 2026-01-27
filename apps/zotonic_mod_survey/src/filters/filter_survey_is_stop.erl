%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013-2026 Marc Worrell
%% @doc Check if there is a 'stop' question in list of (survey) blocks
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

-module(filter_survey_is_stop).
-moduledoc("
Check if there is a ‘stop’ question in list of (survey) blocks

See [mod\\_survey](/id/doc_module_mod_survey)

Todo

Not yet documented.
").

-export([
    survey_is_stop/2
]).

survey_is_stop(Qs, _Context) ->
	lists:any(fun is_stop/1, Qs).

is_stop(#{ <<"type">> := <<"survey_stop">> }) -> true;
is_stop(#{ <<"type">> := <<"survey_page_options">>, <<"is_stop_page">> := true }) -> true;
is_stop(_) -> false.
