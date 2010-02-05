%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010 Arjan Scherpenisse
%% @doc Publish/subscribe admin interface

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

-module(resource_admin_pubsub).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([
    is_authorized/2
]).

-include_lib("resource_html.hrl").

is_authorized(ReqData, Context) ->
    z_auth:wm_is_authorized(ReqData, Context).


html(Context) ->
	Html = z_template:render("admin_pubsub.tpl", [{page_admin_pubsub, true}], Context),
	z_context:output(Html, Context).
