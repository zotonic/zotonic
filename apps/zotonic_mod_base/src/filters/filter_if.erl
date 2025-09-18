%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'if' filter, depending on input select first or second argument

%% Copyright 2011 Marc Worrell
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

-module(filter_if).
-moduledoc("
See also

[if](/id/doc_template_tag_tag_if), [if\\_undefined](/id/doc_template_filter_filter_if_undefined)

Selects an argument depending on a condition.

For example:


```django
{{ value|if:\"yes\":\"no\" }}
```

This is a shortcut for using the [if](/id/doc_template_tag_tag_if) tag. The same can be expressed as follows:


```django
{% if value %}yes{% else %}no{% endif %}
```

Note that falsy values (0, `false`, `undefined` or empty string) evaluate to false.



Elaborate examples
------------------


```django
{% with is_i18n|if:r.translation[lang_code].body:r.body as body %}
```

So if `is_i18n` evaluates to true, `body` is assigned to `r.translation[lang_code].body`, else to `r.body`.


```django
{% include \"_language_attrs.tpl\" id=pid class=(pid==id)|if:\"active\":\"\" %}
```

Add parameter `class` to the included template; when `pid` equals `id`, `class` is `\"active\"`, otherwise an empty string.
").
-export(['if'/4]).


'if'(Input, A, B, _Context) ->
    case z_convert:to_bool(Input) of
        true -> A;
        false -> B
    end.
