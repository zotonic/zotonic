%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse

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

-module(action_admin_zlink).
-moduledoc(#{
    zotonic_keywords => ["reference", "frontend_developer", "wire_action", "content_authoring", "content_relationships", "edit"]
}).
-moduledoc("
Used for inserting an internal link in the TinyMCE editor in the admin.

The required `id` argument identifies the selected resource. The action looks
up its translated title and page URL, then passes both to the editor's
`window.z_zlink` callback.

This is an internal callback action used by the admin link chooser; it normally
does not need to be wired directly by site templates.
").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([render_action/4]).

render_action(_TriggerId, _TargetId, Args, Context) ->
    Id   = z_convert:to_integer(proplists:get_value(id, Args, "")),
    Title = z_trans:lookup_fallback(m_rsc:p(Id, title, Context), Context),
    Url = m_rsc:p(Id, page_url, Context),
	Script = [<<"window.z_zlink(\"">>,z_utils:js_escape(Url),<<"\",\"">>, z_utils:js_escape(Title),<<"\");">>],
	{Script, Context}.
