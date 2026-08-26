%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011-2026 Marc Worrell
%% @doc Convert markdown to/from html.
%% @end

%% Copyright 2011-2026 Marc Worrell
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

-export_type([options/0]).


-type legacy_option() :: no_html | no_tables.
-type options() :: default
                 | faithful
                 | email
                 | markupz:options()
                 | [legacy_option()].


%% @doc Convert HTML to Markdown using the default `markupz' configuration.
-spec to_markdown(Html) -> Markdown when
    Html :: unicode:chardata(),
    Markdown :: binary().
to_markdown(Html) ->
    markupz:to_markdown(Html).

%% @doc Convert HTML to Markdown using a `markupz' preset or option map.
%% The legacy options `no_html' and `no_tables' are accepted for compatibility.
-spec to_markdown(Html, Options) -> Markdown when
    Html :: unicode:chardata(),
    Options :: options(),
    Markdown :: binary().
to_markdown(Html, Options) ->
    markupz:to_markdown(Html, markupz_options(Options)).


%% @doc Convert Markdown to HTML using `markdownz'.
-spec to_html(Markdown) -> Html when
    Markdown :: unicode:chardata(),
    Html :: binary().
to_html(Markdown) when is_list(Markdown) ->
    markdownz:to_binary(unicode:characters_to_binary(Markdown, utf8));
to_html(Markdown) when is_binary(Markdown) ->
    markdownz:to_binary(Markdown).


-spec markupz_options(options()) -> default | faithful | email | markupz:options().
markupz_options(Options) when is_list(Options) ->
    lists:foldl(fun markupz_option/2, #{}, Options);
markupz_options(Options) ->
    Options.

-spec markupz_option(legacy_option(), markupz:options()) -> markupz:options().
markupz_option(no_html, Options) ->
    Options#{html => strip};
markupz_option(no_tables, Options) ->
    Options#{tables => text}.
