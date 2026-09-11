%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% @doc Add a media item to a resource.
%% @end

%% Copyright 2009 Arjan Scherpenisse
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

-module(action_admin_zmedia).
-moduledoc(#{
    zotonic_keywords => ["reference", "frontend_developer", "wire_action", "content_authoring", "media_management", "edit"]
}).
-moduledoc("
Used for triggering the insertion of a media item in the TinyMCE editor in the admin.

The action opens `_action_dialog_zmedia_choose.tpl`. Its arguments are passed to
that dialog, including the current resource `id`, `subject_id`, and the editor's
`media_div_id` when supplied.

```django
{% button text=\"Add media\" action={zmedia id=id subject_id=id media_div_id=\"body-media\"} %}
```

The chooser finishes through the internal `zmedia_choose` and
`zmedia_has_chosen` actions.
").
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").
-include_lib("zotonic_core/include/zotonic.hrl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

render_action(TriggerId, TargetId, Args, Context) ->
    Postback = {zmedia_choose, Args},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Show the add/edit media dialog
event(#postback{message={zmedia_choose, Args}}, Context) ->
    z_render:dialog("Add/edit media", "_action_dialog_zmedia_choose.tpl", Args, Context).

%z_render:wire([{growl, [{text, "Yay."}]}], Context).
