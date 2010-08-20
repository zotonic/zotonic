%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010 Arjan Scherpenisse
%% @doc Post URLs which get transformed into images.

%% Copyright 2010 Arjan Scherpenisse <arjan@scherpenisse.net>
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

-module(resource_imageclipper_go).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([
         is_authorized/2,
         allowed_methods/2,
         process_post/2
]).

-include_lib("resource_html.hrl").

%% @todo Change this into "visible" and add a view instead of edit template.
is_authorized(ReqData, _Context) ->
    Context = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:ensure_all(Context),
    case z_context:get_q("referer", Context1) of
        undefined -> nop;
        _ ->
            z_context:set_persistent(clipper_referer, z_context:get_q("referer", Context1), Context1),
            z_context:set_persistent(clipper_urls, z_context:get_q_all("url", Context1), Context1)
    end,
    z_acl:wm_is_authorized(use, mod_imageclipper, ReqData, Context1).


%% POST just redirect to the GET variant ( the post variables have been stored in the session )
process_post(ReqData, Context) ->
    Redirect = "/clipper/go",
    ReqData1 = wrq:set_resp_header("Location", Redirect, ReqData),
    {{halt, 301}, ReqData1, Context}.


html(Context) ->
    %%Context = z_context:ensure_all(Context0),

    ?DEBUG(z_context:get_persistent(clipper_referer, Context)),
    ?DEBUG(z_context:get_persistent(clipper_urls, Context)),

    Html = z_template:render({cat, "clipper_go.tpl"}, [], Context),
	z_context:output(Html, Context).

allowed_methods(ReqData, Context) ->
    {['POST', 'GET'], ReqData, Context}.

