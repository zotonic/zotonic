%% Copyright 2026 Marc Worrell
%% SPDX-License-Identifier: Apache-2.0
-module(controller_agent_content).
-moduledoc(#{zotonic_keywords => ["reference", "backend_developer", "controller", "http"]}).
-moduledoc("Anonymous GET/HEAD projections. Recompute authorization and content before
conditional requests; caches must revalidate every use. Errors are never cached.").
-export([service_available/1, allowed_methods/1, content_types_provided/1,
    resource_exists/1, generate_etag/1, process/4, markdown/1]).

-spec service_available(z:context()) -> {boolean(), z:context()}.
service_available(C) ->
    Public = z_context:set_noindex_header(z_acl:anondo(C)),
    {true, z_context:set_resp_header(<<"cache-control">>, <<"public, max-age=0, must-revalidate">>, Public)}.

-spec allowed_methods(z:context()) -> {[binary()], z:context()}.
allowed_methods(C) -> {[<<"GET">>, <<"HEAD">>], C}.

-spec content_types_provided(z:context()) -> {[binary()], z:context()}.
content_types_provided(C) ->
    Type = case z_context:get(agent_operation, C) of markdown -> <<"text/markdown">>; _ -> <<"application/json">> end,
    {[Type], C}.

%% Cowmachine invokes this before evaluating If-None-Match (including '*').
-spec resource_exists(z:context()) -> {boolean() | {halt, integer()}, z:context()}.
resource_exists(C0) ->
    C = z_context:ensure_qs(C0),
    Operation = z_context:get(agent_operation, C),
    Op = case Operation of markdown -> resource; _ -> Operation end,
    %% Use only the URI query, excluding injected dispatch/controller arguments.
    Qs = try cow_qs:parse_qs(cowmachine_req:qs(C))
        catch _:_ -> [{<<"invalid_query">>, true}] end,
    Args = maps:from_list(Qs),
    Result = case map_size(Args) =:= length(Qs) of
        false -> {error, bad_request};
        true -> m_agent_content:read(Op, z_context:get_q(<<"id">>, C), Args, C)
    end,
    case Result of
        {ok, Value} ->
            Body = case Operation of
                markdown -> markdown(Value);
                _ -> z_json:encode(#{status => ok, result => Value})
            end,
            case byte_size(Body) =< 1048576 of
                true -> {true, z_context:set(agent_body, Body, C)};
                false -> failure(too_large, C)
            end;
        {error, Reason} -> failure(Reason, C)
    end.

-spec generate_etag(z:context()) -> {binary(), z:context()}.
generate_etag(C) ->
    {binary:encode_hex(crypto:hash(sha256, z_context:get(agent_body, C))), C}.

-spec process(binary(), term(), term(), z:context()) -> {binary(), z:context()}.
process(_, _, _, C) -> {z_context:get(agent_body, C), C}.

-spec markdown(map()) -> binary().
markdown(#{title := Title, page_url := Page, uri := Uri, markdown := Text} = R) ->
    iolist_to_binary([
        "# ", z_markdown:to_markdown(z_html:escape(Title), faithful), "\n\n",
        "Source: ", Page, "\n\nURI: ", Uri, "\n\n",
        "Language: ", atom_to_binary(maps:get(language, R)), "\n\n",
        maps:get(summary, R), "\n\n", Text, "\n\n",
        case maps:get(content_complete, R) of true -> []; false -> "Content is incomplete; follow the source page.\n\n" end,
        "License: ", maps:get(license, R, <<>>), "\n\n",
        "Attribution: ", maps:get(attribution, R, <<>>), "\n\n",
        "Related resources: ", maps:get(related_url, R), "\n\n",
        case maps:get(media, R, undefined) of
            #{download_url := Download} -> ["Media download: ", Download, "\n"];
            _ -> []
        end
    ]).

failure(Reason, C) ->
    Status = case Reason of
        bad_request -> 400;
        not_found -> 404;
        too_large -> 413;
        timeout -> 504;
        _ -> 503
    end,
    Body = z_json:encode(#{status => error, error => Reason}),
    C1 = z_context:set_resp_header(<<"content-type">>, <<"application/json">>, z_context:set_nocache_headers(C)),
    {{halt, Status}, cowmachine_req:set_resp_body(Body, C1)}.
