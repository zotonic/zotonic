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
-moduledoc("
This tag is the placeholder where all generated JavaScript scripts will be output on the page.

Zotonic generates JavaScript for the actions and other template logic. This script needs to be added to the page. The
`{% script
%}` scomp designates the place where the `<script\\>` element with all the generated javascript can be placed.

Normally the `{% script %}` scomp is placed at the end of the page, just above the `</body\\>`.

Note that all JavaScripts generated after the `{% script %}` scomp will not be included in the generated page. Only a
single `{% script
%}` scomp is allowed on any page.

Example:


```django
    {# at the bottom of the template ... #}
    {% script %}
  </body>
</html>
```

This will generate something similar to:


```django
<script type='text/javascript'>
$(function() {
z_pageid=\"MouKz4PgrcROM5efU8iL\";
z_postback_loop();

$('#vtretq').bind('click', function(event) { window.location = \"/admin/edit/647\"; return z_opt_cancel(this); } );
z_init_postback_forms();
z_default_form_postback = \"bVcYISt9JOG/AQZkP9SOZmc//GqDaAVrAAZzdWJtaXRkAAl1bmRlZmluZWRkAAl1bmRlZmluZWRqZAANcmVzb3VyY2VfcGFnZQ==\";
});
</script>
  </body>
</html>
```

Note that the contents of this block will be completely different per page.

The script scomp can have the following arguments:

| Argument  | Description                                                                      | Example       |
| --------- | -------------------------------------------------------------------------------- | ------------- |
| nostartup | Exclude the page initialization code from the script, only includes the scripts from actions etc. Default is to include the page initialization code. | nostartup     |
| nostream  | Do not start the bi-directional communication layer (over WebSockets or comet).  | nostream      |
| format    | Select a different format than the `<script/\\\\>` tag. For now this accepts `\"html\"` (for the `<script/\\\\>` tag), `\"escapejs\"` for an escaped javascript string, and `\"js\"` for a normal javascript string. Default is `\"html\"`. | format=”html” |



WebSockets / Comet communication
--------------------------------

Unless `nostream` is added as a parameter, this tag also causes the WebSockets or Comet communication layer to be initiated.

When available, a WebSocket connections is opened, otherwise a long polling Comet connection is started. The WebSockets
connection will also be used for sending Ajax requests to the server.

See also [Transport](/id/doc_developerguide_server_browser_interaction#guide-transport) for details on the
bi-directional communication.
").
-behaviour(zotonic_scomp).

-export([vary/2, render/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

vary(_Params, _Context) -> nocache.
render(Params, _Vars, _Context) ->
    {ok, {script, Params}}.
