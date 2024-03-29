%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011-2022 Marc Worrell
%% @doc Convert markdown to/from html.
%% @end

%% Copyright 2011-2022 Marc Worrell
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

-module(z_markdown).

-export([
    to_html/1,
    to_markdown/1,
    to_markdown/2
]).


to_markdown(Html) ->
    z_html2markdown:convert(Html).

to_markdown(Html, Options) ->
    z_html2markdown:convert(Html, Options).


to_html(Markdown) when is_list(Markdown) ->
    markdown:conv(unicode:characters_to_binary(Markdown, utf8));
to_html(Markdown) when is_binary(Markdown) ->
    markdown:conv(Markdown).
