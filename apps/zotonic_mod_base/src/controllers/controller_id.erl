%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2023 Marc Worrell <marc@worrell.nl>
%% @doc Redirect to resources depening on the content type requested.
%% @end

%% Copyright 2023 Marc Worrell
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

-module(controller_id).
-moduledoc("
Handle different content representations of a page.

Redirects to different representations of a page, depending on the requested content type. The redirect is done using a
“303 See Other” status. A “404 Not Found” or “410 Gone” is returned if the requested page never existed or
has been deleted.

When no content types are requested then `text/html` is selected.

This controller is also used for a page’s short url representation.

Example dispatch rule (from mod_base):


```erlang
{id, [\"id\", id], controller_id, []}
```

This controller does not have any dispatch options.

This controller handles the following query argument:

| Option | Description                                                     | Example URL |
| ------ | --------------------------------------------------------------- | ----------- |
| id     | Id of the requested [resource](/id/doc_glossary#term-resource). | /id/1234    |

The list of provided content types is collected with a foldr notification (see
[Notifications](/id/doc_developerguide_notifications#guide-notification)) of the type `content_types_dispatch`. Modules
should add their provided content types in front of the accumulator. The added entries are tuples: `{MimeType, DispatchRuleName}`.

Example of adding a content type handler adding a text/plain handler with the dispatch rule `rsc_text`:


```erlang
observe_content_types_dispatch(#content_types_dispatch{}, Acc, _Context) ->
    [{\"text/plain\", rsc_text} | Acc].
```
").
-author("Marc Worrell <marc@worrell.nl>").

-export([
    resource_exists/1,
    previously_existed/1,
    moved_permanently/1,
    content_types_provided/1,
    process/4,
    get_rsc_content_types/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

resource_exists(Context) ->
    {Id, ContextQs} = get_id(z_context:ensure_qs(Context)),
    z_context:logger_md(ContextQs),
    {m_rsc:exists(Id, ContextQs), ContextQs}.

previously_existed(Context) ->
    {Id, Context2} = get_id(Context),
    {m_rsc_gone:is_gone(Id, Context2), Context2}.

moved_permanently(Context) ->
    {Id, Context2} = get_id(Context),
    redirect(m_rsc_gone:get_new_location(Id, Context2), Context2).

redirect(undefined, Context) ->
    {false, Context};
redirect(Location, Context) ->
    {{true, Location}, Context}.

content_types_provided(Context) ->
    {CTs, Context1} = get_content_types(Context),
    CTs1 = [ Mime || {Mime, _Dispatch} <- CTs],
    {CTs1, Context1}.

process(_Method, _AcceptedCT, ProvidedCT, Context) ->
    {CT, Context2} = get_content_types(Context),
    {Id, Context3} = get_id(Context2),
    Location = case proplists:get_value(ProvidedCT, CT) of
        page_url ->
            m_rsc:p_no_acl(Id, page_url, Context3);
        {url, Url} ->
            Url;
        {Dispatch, DispatchArgs} when is_atom(Dispatch), is_list(DispatchArgs) ->
            z_dispatcher:url_for(Dispatch, [{id,Id} | DispatchArgs], Context3);
        Dispatch when is_atom(Dispatch) ->
            z_dispatcher:url_for(Dispatch, [{id,Id}], Context3)
    end,
    AbsUrl = z_context:abs_url(Location, Context3),
    Context4 = z_context:set_resp_header(<<"location">>, AbsUrl, Context3),
    Context5 = z_context:set_resource_headers(Id, Context4),
    {{halt, 303}, Context5}.

%% @doc Fetch the list of content types provided, together with their dispatch rule name.
%% text/html is moved to the front of the list as that is the default mime type to be returned.
-spec get_rsc_content_types(m_rsc:resource(), z:context()) -> list().
get_rsc_content_types(Id, Context) ->
    z_notifier:foldr(#content_types_dispatch{id = Id}, [], Context).

get_content_types(Context) ->
    case z_context:get(content_types_dispatch, Context) of
        undefined ->
            {Id, Context1} = get_id(Context),
            CTs = get_rsc_content_types(Id, Context),
            CTs1 = case find_html(CTs) of
                    undefined -> [ {{<<"text">>, <<"html">>, []}, page_url} | CTs ];
                    Prov -> [ Prov | CTs ]
                  end,
            Context2 = z_context:set(content_types_dispatch, CTs1, Context1),
            {CTs1, Context2};
        CTs ->
            {CTs, Context}
    end.

find_html([]) ->
    undefined;
find_html([ {{<<"text">>, <<"html">>, _}, _} = Prov | _CTs ]) ->
    Prov;
find_html([ _ | CTs ]) ->
    find_html(CTs).

% Fetch id from the request. Check if the id has an extension, if so split it
% and set the accept header.
get_id(Context) ->
    case z_context:get(id, Context) of
        undefined ->
            case z_context:get_q(<<"id">>, Context) of
                undefined ->
                    {undefined, Context};
                <<>> ->
                    {undefined, Context};
                Id ->
                    case maybe_split_extension(Id) of
                        {Root, Ext} ->
                            RscId = m_rsc:rid(Root, Context),
                            Context1 = z_cowmachine_middleware:set_accept_context(Ext, Context),
                            {RscId, z_context:set(id, {ok, RscId}, Context1)};
                        false ->
                            RscId = m_rsc:rid(Id, Context),
                            {RscId, z_context:set(id, {ok, RscId}, Context)}
                    end
            end;
        {ok, Id} ->
            {Id, Context}
    end.

maybe_split_extension(<<"http:", _/binary>>) ->
    false;
maybe_split_extension(<<"https:", _/binary>>) ->
    false;
maybe_split_extension(Id) ->
    case filename:extension(Id) of
        <<".", Ext/binary>> when Ext =/= <<>> ->
            Root = filename:rootname(Id),
            case is_name_like(Root) of
                true ->
                    {Root, Ext};
                false ->
                    false
            end;
        _ ->
            false
    end.

is_name_like(<<>>) -> true;
is_name_like(<<$_, R/binary>>) -> is_name_like(R);
is_name_like(<<C, _/binary>>) when C < $0 -> false;
is_name_like(<<C, _/binary>>) when C > $9, C < $A -> false;
is_name_like(<<C, _/binary>>) when C > $Z, C < $a -> false;
is_name_like(<<C, _/binary>>) when C > $z, C < 128 -> false;
is_name_like(<<_, R/binary>>) -> is_name_like(R).
