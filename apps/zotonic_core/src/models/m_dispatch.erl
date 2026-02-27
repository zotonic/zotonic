%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2025-2026 Marc Worrell
%% @doc Model for dispatching requests.
%% @end

%% Copyright 2025-2026 Marc Worrell
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

-module(m_dispatch).
-moduledoc("
Dispatch or generate URLs or page paths. Useful to check dispatch rules or for client side code to dispatch page paths.



Dispatching URLs
----------------

The `path` returns the matched dispatch rule, controller, controller options, language, bindings and other dispatch information.

If no dispatch rule matches then 404 Not Found is returned.

Example:


```bash
curl -k 'https://example.test:8443/api/model/dispatch/get/path/nl/page/1234/hello'
```

Returns:


```json
{
    \"result\": {
        \"bindings\": {
            \"id\": \"1234\",
            \"z_language\": \"nl\",
            \"zotonic_site\": \"example\",
            \"zotonic_dispatch_path\": [
                \"page\",
                \"1234\",
                \"hello\"
            ],
            \"zotonic_dispatch\": \"page\",
            \"slug\": \"hello\"
        },
        \"controller\": \"controller_page\",
        \"site\": \"example\",
        \"language\": \"nl\",
        \"dispatch_rule\": \"page\",
        \"path_tokens\": [
            \"page\",
            \"1234\",
            \"hello\"
        ],
        \"controller_options\": {
            \"template\": {
                \"template\": \"page.tpl\",
                \"is_catinclude\": true
            },
            \"zotonic_dispatch_file\": \"dispatch\",
            \"zotonic_dispatch_module\": \"mod_site_whatwebwhat\"
        }
    },
    \"status\": \"ok\"
}
```



Creating URLs
-------------

URLs or page paths can also be requested using the `url_for` method. Here the name of the dispatch rule is padded,
optionally with any extra arguments.

Example:


```bash
curl -k 'https://example.test:8443/api/model/dispatch/get/url_for/admin'
```

Returns:


```json
{\"result\":\"/en/admin\",\"status\":\"ok\"}
```

Or, use `abs_url_for` if a complete URL with domain is needed:


```bash
curl -k 'https://example.test:8443/api/model/dispatch/get/abs_url_for/admin'
```

Returns:


```json
{\"result\":\"https://example.com/en/admin\",\"status\":\"ok\"}
```

And for more complex URLs:


```bash
curl -k 'https://example.test:8443/api/model/dispatch/get/url_for/page?id=1234&slug=foo&a=1&bar=baz'
```

Returns:


```json
{\"result\":\"/en/page/1234/foo?a=1&bar=baz\",\"status\":\"ok\"}
```

Here the selected dispatch rule was something like:


```erlang
{page, [ \"page\", id, slug ], controller_page, [ {template, \"page.tpl\"} ]}
```

Because `id` and `slug` are part of the path, they are included in the generated URL path, the other arguments are added
as query parameters.

Available Model API Paths
-------------------------

| Method | Path pattern | Description |
| --- | --- | --- |
| `get` | `/url` | Dispatch full URL from payload field `url`, returning matched rule/controller/options/bindings for this site only (`enoent` on mismatch or no match). No further lookups. |
| `get` | `/url/+url` | Dispatch full URL `+url`, returning matched rule/controller/options/bindings when it resolves to the current site; otherwise `enoent`. No further lookups. |
| `get` | `/path` | Dispatch path from payload field `path`, returning matched rule/controller/options/bindings (`enoent` when no rule matches). No further lookups. |
| `get` | `/path/...` | Dispatch supplied path segments (percent-encoded and joined) and return matched rule/controller/options/bindings. |
| `get` | `/url_for/+name/...` | Generate relative URL for dispatch rule `+name` using payload args (path vars consumed by rule, remaining args as query params); `enoent` when rule is unknown. |
| `get` | `/abs_url_for/+name/...` | Generate absolute URL for dispatch rule `+name` from `url_for` result and current site host/scheme; `enoent` when rule is unknown. |

`/+name` marks a variable path segment. A trailing `/...` means extra path segments are accepted for further lookups.
").
-author("Marc Worrell <marc@worrell.nl").

-behaviour(zotonic_model).

-export([
    m_get/3,

    dispatch_url/2,
    dispatch_path/2
]).

m_get([ <<"url">> ], #{ payload := #{ <<"url">> := Url } }, Context) ->
    dispatch_url(Url, Context);
m_get([ <<"url">>, Url ], _Msg, Context) ->
    dispatch_url(Url, Context);
m_get([ <<"path">> ], #{ payload := #{ <<"path">> := Path } }, Context) ->
    dispatch_path(Path, Context);
m_get([ <<"path">> | Path ], _Msg, Context) ->
    dispatch_path(Path, Context);
m_get([ <<"url_for">>, Name | Rest ], #{ payload := Payload }, Context) ->
    case url_for(Name, Payload, Context) of
        undefined -> {error, enoent};
        Url -> {ok, {Url, Rest}}
    end;
m_get([ <<"abs_url_for">>, Name | Rest ], #{ payload := Payload }, Context) ->
    case url_for(Name, Payload, Context) of
        undefined -> {error, enoent};
        Url -> {ok, {z_context:abs_url(Url, Context), Rest}}
    end;
m_get(_, _Msg, _Context) ->
    {error, enoent}.

url_for(Name, undefined, Context) ->
    z_dispatcher:url_for(Name, Context);
url_for(Name, Args, Context) when is_map(Args) ->
    url_for(Name, maps:to_list(Args), Context);
url_for(Name, Args, Context) when is_list(Args) ->
    ArgsList = lists:map(
        fun
            ({K, V} = KV) when is_binary(K) ->
                try
                    {binary_to_existing_atom(K, utf8), V}
                catch
                    _:_ -> KV
                end;
            ({K, _V} = KV) when is_atom(K) ->
                KV;
            (K) when is_atom(K) ->
                {K, true};
            (K) when is_binary(K) ->
                try
                    {binary_to_existing_atom(K, utf8), true}
                catch
                    _:_ -> {K, true}
                end
        end,
        Args),
    z_dispatcher:url_for(Name, ArgsList, none, Context).

dispatch_url(undefined, _Context) ->
    {error, enoent};
dispatch_url(Url, Context) when is_binary(Url) ->
    case z_sites_dispatcher:dispatch_url(Url) of
        {ok, #{
            context := UrlContext
        } = Disp} ->
            Site = z_context:site(Context),
            UrlSite = z_context:site(UrlContext),
            if
                Site =:= UrlSite -> cleanup_disp(Disp);
                true -> {error, enoent}
            end;
        {error, _} ->
            {error, enoent}
    end;
dispatch_url(_, _Context) ->
    {error, enoent}.

dispatch_path(undefined, _Context) ->
    {error, enoent};
dispatch_path(Path, Context) when is_list(Path) ->
    Path1 = iolist_to_binary([ [ <<"/">>, z_url:percent_encode(P) ] || P <- Path ]),
    dispatch_path(Path1, Context);
dispatch_path(Path, Context) when is_binary(Path) ->
    case z_sites_dispatcher:dispatch_path(Path, Context) of
        {ok, Disp} -> cleanup_disp(Disp);
        {error, _} -> {error, enoent}
    end;
dispatch_path(_, _Context) ->
    {error, enoent}.

cleanup_disp(#{ context := DispContext} = Disp) ->
    Disp1 = maps:without([ context ], Disp),
    Disp2 = Disp1#{
        language => z_context:language(DispContext)
    },
    Disp3 = maybe_encode_strings(Disp2),
    {ok, {Disp3, []}}.

maybe_encode_strings(#{ controller_options := Opts } = Disp) ->
    Opts1 = lists:map(
        fun
            ({K, [ C | _ ] = V}) when is_integer(C) ->
                try
                    {K, unicode:characters_to_binary(V, utf8)}
                catch
                    _:_ -> {K, V}
                end;
            ({template, {cat, Tpl}}) ->
                {template, #{
                    is_catinclude => true,
                    template => unicode:characters_to_binary(Tpl, utf8)
                }};
            (KV) -> KV
        end,
        Opts),
    Disp#{
        controller_options => Opts1
    };
maybe_encode_strings(Disp) ->
    Disp.
