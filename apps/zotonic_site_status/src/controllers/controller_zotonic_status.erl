%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2019 Marc Worrell
%% @doc Resource to serve the zotonic fallback site templates.

%% Copyright 2010-2019 Marc Worrell
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

-module(controller_zotonic_status).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    content_types_provided/1,
    is_authorized/1,
	process/4,
	event/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_wires/include/mod_wires.hrl").


content_types_provided(Context) ->
    case z_context:get(content_type, Context) of
        undefined ->
            {[ {<<"text">>, <<"html">>, []} ], Context};
        Mime when is_list(Mime) ->
            {[ z_convert:to_binary(Mime) ], Context};
        Mime ->
            {[ Mime ], Context}
    end.

is_authorized(Context) ->
    case z_context:get(is_fallback_template, Context) of
        true -> {true, Context};
        _ -> {z_acl:is_admin(Context), Context}
    end.

process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    case zotonic_site_status:is_peer_allowed(Context) of
        false ->
            render_page(false, "logon.tpl", Context);
        true ->
            case z_context:get(is_fallback_template, Context) of
                true ->
                    Template = z_context:get(template, Context),
                    render_page(true, Template, Context);
                undefined ->
                    case z_acl:is_admin(Context) of
                        false -> render_page(true, "logon.tpl", Context);
                        true -> status_page(true, Context)
                    end
            end
    end.

render_page(IsPeerAllowed, Template, Context) ->
    StatusCode = resp_code(Context),
    Vars = [
        {error_code, StatusCode},
        {is_peer_allowed, IsPeerAllowed}
        | z_context:get_all(Context)
    ],
    Rendered = z_template:render(Template, Vars, Context),
    {Output, OutputContext} = z_context:output(Rendered, Context),
    Context1 = cowmachine_req:set_response_code(StatusCode, OutputContext),
    Context2 = cowmachine_req:set_resp_body(Output, Context1),
    Context3 = case StatusCode of
        200 ->
            z_context:set_noindex_header(
                z_context:set_nocache_headers(Context2)
            );
        _ ->
            Context2
    end,
    {{halt, StatusCode}, Context3}.

resp_code(Context) ->
    case z_context:get(is_fallback_template, Context) of
        true ->
            case z_context:get_q(http_status_code, Context) of
                StatusCode when is_integer(StatusCode) ->
                    StatusCode;
                _ ->
                    case cowmachine_req:get_metadata(http_status_code, Context) of
                        undefined -> 200;
                        StatusCode -> StatusCode
                    end
            end;
        undefined ->
            200
    end.

status_page(IsPeerAllowed, Context) ->
    Template = z_context:get(template, Context),
    Vars = [
        {is_peer_allowed, IsPeerAllowed}
    ] ++  z_context:get_all(Context),
    Rendered = z_template:render(Template, Vars, Context),
    z_context:output(Rendered, Context).


%% -----------------------------------------------------------------------------------------------
%% Handle all events
%% -----------------------------------------------------------------------------------------------

event(#postback{message={site_start, [{site, Site}]}}, Context) when is_atom(Site) ->
    true = z_auth:is_auth(Context),
    z_sites_manager:start(Site),
    render_notice(Site, "Successfully started.", Context);
event(#postback{message={site_restart, [{site, Site}]}}, Context) when is_atom(Site) ->
    true = z_auth:is_auth(Context),
    z_sites_manager:restart(Site),
    render_notice(Site, "Successfully restarted.", Context);
event(#postback{message={site_stop, [{site, Site}]}}, Context) when is_atom(Site) ->
    true = z_auth:is_auth(Context),
    z_sites_manager:stop(Site),
    render_notice(Site, "Successfully stopped.", Context);
event(#postback{message={site_flush, [{site, Site}]}}, Context) when is_atom(Site) ->
    true = z_auth:is_auth(Context),
    zotonic_fileindexer:flush(),
    z:flush(z_context:new(Site)),
    render_notice(Site, "The cache is flushed and all dispatch rules are reloaded.", Context);
event(#postback{message={site_admin, [{site,Site}]}}, Context) when is_atom(Site) ->
    true = z_auth:is_auth(Context),
    try
        SiteContext = z_context:new(Site),
        case z_dispatcher:url_for(admin, SiteContext) of
            undefined ->
                z_render:growl_error("This site does not have an admin url.", Context);
            U ->
                Url = z_dispatcher:abs_url(U, SiteContext),
                z_render:wire({redirect, [{location,Url}]}, Context)
        end
    catch
        _:_ -> z_render:growl_error("Could not fetch the admin url, the site might not be running.", Context)
    end.


% %% -----------------------------------------------------------------------------------------------
% %% Stream process to update the page when data changes
% %% -----------------------------------------------------------------------------------------------

render_notice(Site, Notice, Context) ->
    z_render:wire( notice(Site, Notice), Context ).

% @doc Actions to show a notice.
-spec notice(atom(), iodata()) -> list().
notice(SiteName, Text) ->
    [
        {insert_top, [
            {target, "notices"},
            {template, "_notice.tpl"},
            {site, SiteName},
            {notice, Text}
        ]},
        {fade_out, [
            {selector, "#notices > div:gt(0)"},
            {speed, 2000}
        ]}
    ].
