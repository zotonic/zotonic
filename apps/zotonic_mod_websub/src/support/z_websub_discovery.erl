%% @copyright 2026 Marc Worrell
%% @doc WebSub discovery. Resource identity is always the advertised self URI,
%% never the final representation URL reached through content negotiation.
-module(z_websub_discovery).
-moduledoc("Discover WebSub links in HTTP headers, then HTML/XML link elements.").
-export([discover/2, links/4, is_url/1, normalize_url/1]).

-spec discover(Url, Context) -> {ok, map()} | {error, term()} when
    Url :: binary(), Context :: z:context().
discover(Url, Context) ->
    discover(Url, 0, Context).

discover(_, 5, _) ->
    {error, too_many_redirects};
discover(Url, Count, Context) ->
    case is_url(Url) of
        true ->
            Options = [{accept, <<"application/json, text/html, application/atom+xml">>},
                {autoredirect, false}, {timeout, 10000}, {max_length, 1048576}],
            case z_websub_http:fetch(get, Url, <<>>, Options, Context) of
                {ok, {Final, Headers, _, Body}} ->
                    links(Final, Headers, Body, undefined);
                {error, {Code, Final, Headers, _, Body}}
                    when Code =:= 301; Code =:= 302; Code =:= 303; Code =:= 307; Code =:= 308 ->
                    % /id is a semantic identifier and commonly advertises its links
                    % on a 303 response before redirecting to a representation.
                    case links(Final, Headers, Body, undefined) of
                        {ok, _} = Found ->
                            Found;
                        _ ->
                            discover_redirect(Url, Headers, Count, Context)
                    end;
                {error, {Code, _, _, _, _}} when is_integer(Code) ->
                    {error, {http_status, Code}};
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, invalid_url}
    end.

discover_redirect(Url, Headers, Count, Context) ->
    Location = proplists:get_value("location", Headers, proplists:get_value(<<"location">>, Headers)),
    try z_convert:to_binary(uri_string:resolve(z_convert:to_binary(Location), Url)) of
        Next ->
            NextContext = case origin(Url) =:= origin(Next) of
                true ->
                    Context;
                false ->
                    z_acl:anondo(z_context:new(Context))
            end,
            case {Url, Next} of
                {<<"https:", _/binary>>, <<"http:", _/binary>>} ->
                    {error, insecure_redirect};
                _ ->
                    discover(Next, Count + 1, NextContext)
            end
    catch _:_ ->
        {error, invalid_redirect} end.

origin(Url) ->
    #{host := Host, scheme := Scheme} = Parts = uri_string:parse(Url),
    {Scheme, Host, maps:get(port, Parts, case Scheme of <<"https">> -> 443; _ -> 80 end)}.

%% ExportLinks is the optional Zotonic JSON discovery extension. Headers take precedence.
-spec links(Url, Headers, Body, ExportLinks) -> {ok, map()} | {error, term()} when
    Url :: binary() | string(), Headers :: list(), Body :: binary(), ExportLinks :: term().
links(Url, Headers, Body, ExportLinks) ->
    LinkHeaders = [z_convert:to_binary(V) || {K, V} <- Headers,
        z_string:to_lower(z_convert:to_binary(K)) =:= <<"link">>],
    try
        Links = case LinkHeaders of
            [] ->
                case ExportLinks of
                    L when is_list(L) ->
                        L;
                    _ ->
                        body_links(Body, Headers)
                end;
            _ ->
                lists:append([cow_link:parse_link(H) || H <- LinkHeaders])
        end,
        Self = urls(<<"self">>, Links, Url),
        Hubs = urls(<<"hub">>, Links, Url),
        case {Self, Hubs} of
            {[Topic], [_ | _]} ->
                {ok, #{topic => Topic, hubs => Hubs}};
            _ ->
                {error, no_websub}
        end
    catch
        _:_ ->
            {error, invalid_discovery}
    end.

urls(Rel, Links, Base) ->
    lists:usort([Url || Link <- Links,
        lists:member(Rel, binary:split(z_convert:to_binary(value(rel, Link)), <<" ">>, [global])),
        not lists:keymember(<<"anchor">>, 1, maps:get(attributes, Link, [])),
        Url <- [normalize_url(z_convert:to_binary(uri_string:resolve(value(target, Link), z_convert:to_binary(Base))))],
        is_url(Url)]).

value(Key, Map) ->
    maps:get(Key, Map, maps:get(atom_to_binary(Key), Map, <<>>)).

body_links(Body, Headers) ->
    CT = z_string:to_lower(z_convert:to_binary(proplists:get_value("content-type", Headers,
        proplists:get_value(<<"content-type">>, Headers, <<>>)))),
    case {binary:match(CT, <<"html">>), binary:match(CT, <<"xml">>)} of
        {nomatch, nomatch} ->
            json_links(Body);
        {nomatch, _} ->
            tree_links(mochiweb_html:parse(Body), false);
        _ ->
            tree_links(mochiweb_html:parse(Body), true)
    end.

json_links(Body) ->
    case z_json:decode(Body) of
        #{<<"result">> := #{<<"links">> := Links}} when is_list(Links) ->
            Links;
        #{<<"links">> := Links} when is_list(Links) ->
            Links;
        _ ->
            []
    end.

tree_links({<<"body">>, _, _}, true) ->
    [];
tree_links({<<"entry">>, _, _}, false) ->
    [];
tree_links({<<"item">>, _, _}, false) ->
    [];
tree_links({Tag, Attrs, Children}, Html) ->
    Here = case lists:last(binary:split(Tag, <<":">>, [global])) of
        <<"link">> ->
            [#{rel => proplists:get_value(<<"rel">>, Attrs, <<>>),
                         target => proplists:get_value(<<"href">>, Attrs, <<>>)}];
        _ ->
            []
    end,
    Here ++ lists:append([tree_links(C, Html) || C <- Children]);
tree_links(_, _) ->
    [].

%% @doc RFC 3986 normalization decodes unreserved percent-encoded characters,
%% as required by WebSub, while preserving escaped delimiters such as %2F.
%% Keep invalid input unchanged so the normal URL validator can reject it.
-spec normalize_url(Url) -> term() when
    Url :: term().
normalize_url(Url) when is_binary(Url) ->
    case uri_string:normalize(Url) of
        Normalized when is_binary(Normalized) ->
            Normalized;
        _ ->
            Url
    end;
normalize_url(Url) ->
    Url.

-spec is_url(Url) -> boolean() when
    Url :: term().
is_url(Url) when is_binary(Url), byte_size(Url) > 0, byte_size(Url) =< 500 ->
    case uri_string:parse(Url) of
        #{scheme := Scheme, host := Host} = Parts when Host =/= <<>> ->
            (Scheme =:= <<"https">> orelse Scheme =:= <<"http">>)
                andalso not maps:is_key(userinfo, Parts)
                andalso not maps:is_key(fragment, Parts);
        _ ->
            false
    end;
is_url(_) ->
    false.
