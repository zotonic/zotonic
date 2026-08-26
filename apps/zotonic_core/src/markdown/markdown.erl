%% @copyright 2026 Marc Worrell
%% @doc Compatibility entry point for Markdown to HTML conversion.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(markdown).

-export([conv/1]).
-deprecated({conv, 1}).

%% @deprecated Use `markdownz:to_binary/1' instead.
-spec conv(MarkdownText) -> Html when
    MarkdownText :: iodata(),
    Html :: binary().
conv(MarkdownText) ->
    markdownz:to_binary(MarkdownText).
