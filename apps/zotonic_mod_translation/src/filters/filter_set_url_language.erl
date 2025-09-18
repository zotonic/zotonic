%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2023 Marc Worrell
%% @doc Change the language of an URL to another language
%% @end

%% Copyright 2023 Marc Worrell
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

-module(filter_set_url_language).
-moduledoc("
Change the language of an URL to another language. Useful to generate alternative URLs for a page.

Example, provide an URL to the German version of the current page:


```django
{{ m.req.raw_path|set_url_language:'de' }}
```

The URL is sanitized before the language is added. The returned URL is always an absolute URL with the correct hostname,
port and protocol.

Note that it is possible to get an alternative language version of a resource’s page by providing a `z_language` argument:


```django
{{ id.page_url_abs with z_language = 'de' }}
```
").

-export([
    set_url_language/2,
    set_url_language/3
]).

set_url_language(Url, Context) ->
    set_url_language(Url, z_context:language(Context), Context).

set_url_language(undefined, _Lang, _Context) ->
    undefined;
set_url_language(Url, 'x-default', Context) ->
    Location = z_sanitize:uri(Url),
    z_context:abs_url(
            mod_translation:url_strip_language(Location),
            Context);
set_url_language(Url, <<"x-default">>, Context) ->
    set_url_language(Url, 'x-default', Context);
set_url_language(Url, Lang, Context) ->
    Location = z_sanitize:uri(Url),
    case z_language:is_valid(Lang) of
        true ->
            z_context:abs_url(
                    add_language(mod_translation:url_strip_language(Location), Lang),
                    Context);
        false ->
            Location
    end.

-spec add_language(iodata(), atom()|binary()) -> binary().
add_language(<<>>, Lang) ->
    add_language(<<"/">>, Lang);
add_language(Url, Lang) ->
    iolist_to_binary([$/, z_convert:to_binary(Lang), Url]).
