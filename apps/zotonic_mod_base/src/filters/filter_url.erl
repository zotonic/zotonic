%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020-2023 Marc Worrell
%% @doc 'url' filter, generates an url
%% @end

%% Copyright 2020-2023 Marc Worrell
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

-module(filter_url).
-moduledoc("
Generates the relative URL for the given dispatch information.

An *relative URL* is an URL that excludes the protcol and hostname. For example `/foo/bar`.

For example, generate a url for the dispatch rule `home` with an extra argument `hello`:


```django
{{ {home hello=\"world\"}|url }}
```

This is similar to:


```django
{% url home hello=\"world\" %}
```

Difference between the tag and the filter is that the filter can be used in expressions or with passed values.

See also

[url_abs](/id/doc_template_filter_filter_url_abs), [url](/id/doc_template_tag_tag_url), [sanitize_url](/id/doc_template_filter_filter_sanitize_url), [is_site_url](/id/doc_template_filter_filter_is_site_url), [urlencode](/id/doc_template_filter_filter_urlencode)").
-export([url/2]).

url(undefined, _Context) ->
    undefined;
url(Name, Context) when is_atom(Name) ->
    z_dispatcher:url_for(Name, Context);
url({Name, Args}, Context) when is_atom(Name), is_list(Args) ->
    z_dispatcher:url_for(Name, Args, Context);
url(Name, Context) when is_binary(Name)->
    try binary_to_existing_atom(Name, utf8) of
        Atom -> url(Atom, Context)
    catch
        error:badarg -> undefined
    end;
url(Name, Context) when is_list(Name)->
    try list_to_existing_atom(Name) of
        Atom -> url(Atom, Context)
    catch
        error:badarg -> undefined
    end;
url(_Name, _Context) ->
    undefined.

