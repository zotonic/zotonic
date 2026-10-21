%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% Based on code (c) 2008-2009 Rusty Klophaus

%% Copyright 2009 Marc Worrell
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

-module(scomp_wires_script).
-moduledoc(#{
    zotonic_keywords => ["reference", "frontend_developer", "scomp", "javascript", "render"]
}).
-moduledoc("
Emit the JavaScript collected from template actions, wires, validators and `javascript` blocks.

Place one `script` tag near the end of the page, after all content that generates JavaScript and normally just before `</body>`:

```django
{% javascript %}
    initializePage();
{% endjavascript %}

{% script %}
</body>
</html>
```

In its default HTML format, the tag emits a `<script>` element with the page's CSP nonce and defines `window.zotonicPageInit`. Zotonic's wired JavaScript calls this function during page initialization. Use the standard Cotonic head and wired JavaScript includes.

Browser-server communication uses MQTT over WebSocket connections, managed by
Cotonic. The standard Cotonic head and wired JavaScript includes set up this
connection; the `script` tag supplies the collected page initialization code.

JavaScript collected after this tag is not included in that initialization script. Use a single `script` tag per page and put all declarations that it must process before it.

For example, the generated HTML has this structure (the nonce and function
contents depend on the page):

```html
<script type=\"text/javascript\" nonce=\"...\">
    z_script_nonce = '...';
    window.zotonicPageInit = function() {
        initializePage();
        // Collected actions and standard page startup code.
    };
</script>
```

## Options

| Option | Default | Description |
| --- | --- | --- |
| `nostartup` | `false` | Omit the standard page startup code supplied by script-render observers, retaining collected actions and template JavaScript. Library flushing still occurs. |
| `format` | `\"html\"` | Choose `\"html\"` for a script element, `\"js\"` for JavaScript without the element, or `\"escapejs\"` for an escaped JavaScript string. |

With `format=\"js\"` or `format=\"escapejs\"`, pending libraries become browser-loader dependencies instead of HTML include elements. The generated JavaScript waits for them before running the collected initialization code. These formats require Zotonic's wired asset loader to be available in the receiving page.

## Flushing collected libraries

Before emitting the HTML initialization script, `script` flushes all pending CSS and JavaScript declared with `lib collect`:

```django
{% lib \"css/component.css\" \"js/component.js\" collect %}

<div id=\"component\">...</div>

{% javascript %}
    initializeComponent();
{% endjavascript %}

{% script %}
```

The collected library include elements appear before the initialization script. Their original options and order are preserved; only compatible adjacent groups combine. Duplicate tracking remains intact after the flush.

For earlier or type-specific placement, use explicit lib flush tags:

```django
{% lib flush=\"css\" %}
{% lib flush=\"javascript\" %}
{% lib flush %}
```

`\"js\"` is an alias for `\"javascript\"`. Unlike an ordinary CSS-only or JavaScript-only lib tag, `script` flushes both types. See `tag#lib` for collection rules and option restrictions.

## Dynamic updates

Dynamic fragments normally do not need their own `script` tag. Wired updates collect the fragment's libraries and JavaScript automatically. Missing libraries load before DOM insertion and widget initialization; collected template JavaScript runs after the DOM update.

For template-model requests, use `model/template/get/render_with_assets/<template>` to obtain `html`, `libs` and `javascript`, then pass the result to `z_assets.apply(result, insertHtml)`. The insertion callback may return a promise; initialization waits for it to finish. The HTML-only `render` endpoint does not return the separate initialization information.

The lower-level `z_template:render_to_iolist/3` materializes HTML and returns collected script state in its updated context; it does not expand a `script` placeholder into the page initialization element. Use the normal final page-output path to render that placeholder, or the asset-aware update API for dynamic fragments.

See also `tag#lib` and `tag#javascript`.
").

-behaviour(zotonic_scomp).

-export([vary/2, render/3]).

vary(_Params, _Context) -> nocache.
render(Params, _Vars, _Context) ->
    {ok, {script, Params}}.
