%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% @doc Redirect to a preview for media items; for use in the tinyMCE media plugin.

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

-module(controller_admin_media_preview).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([resource_exists/1,
         content_types_provided/1,
         to_image/1
        ]).

resource_exists(Context) ->
    Context2 = z_admin_controller_helper:init_session(Context),
    case z_context:get_q(<<"id">>, Context2) of
        undefined ->
            {false, Context2};
        <<>> ->
            {false, Context2};
        Id ->
            case m_rsc:rid(Id, Context2) of
                undefined -> 
                    {false, Context2};
                RscId ->
                    case m_rsc:exists(RscId, Context2) andalso m_rsc:is_visible(RscId, Context2) of 
                        true ->
                            Context3 = z_context:set(id, RscId, Context2),
                            {true, Context3};
                        false ->
                            {true, Context2}
                    end
            end
    end.

content_types_provided(Context) ->
    {[{<<"image/jpeg">>, to_image}], Context}.

to_image(Context) ->
    Opts = [{mediaclass, <<"admin-editor">>}],
    case z_media_tag:url(z_context:get(id, Context), Opts, Context) of
        {ok, Url} ->
            Context1 = z_context:set_resp_header(<<"location">>, z_context:abs_url(Url, Context), Context),
            {{halt, 303}, Context1};
        {error, enoent} ->
            {{halt, 404}, Context}
    end.

