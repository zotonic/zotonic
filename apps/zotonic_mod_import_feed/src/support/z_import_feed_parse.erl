%% @copyright 2026 Marc Worrell
%% @doc RSS 2, RDF/RSS 1 and Atom normalization without creating XML-name atoms.

%% Copyright 2026 Marc Worrell
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

-module(z_import_feed_parse).
-define(MAX_FEED_BYTES, 4 * 1024 * 1024).
-define(MAX_ENTRIES, 100).
-define(MAX_IDENTITY_LENGTH, 2048).
-define(ZOTONIC_RESOURCE_REL, <<"https://zotonic.com/rel/resource">>).
-export([parse/2, discover/2, discover/3, html_links/2, identity/3, date/1, images/2, image_variants/2]).

-spec parse(Data, Url) -> {ok, map()} | {error, term()} when
    Data :: binary(), Url :: binary().
parse(Data, Url) when byte_size(Data) =< ?MAX_FEED_BYTES ->
    %% No DTDs or custom entities, including external entities. z_html_parse
    %% retains names as binaries and does not fetch external documents.
    case re:run(Data, <<"<!\\s*(DOCTYPE|ENTITY)" >>, [caseless, {capture, none}]) of
        match -> {error, forbidden_xml_declaration};
        nomatch ->
            try z_html_parse:parse(Data, #{mode => xml, escape => false, lowercase => true}) of
                {ok, {Name, _, _} = Root} -> parse_root(local(Name), Root, Url);
                _ -> {error, invalid_feed}
            catch _:_ -> {error, invalid_feed} end
    end;
parse(_, _) -> {error, response_too_large}.

parse_root(<<"rss">>, Root, Url) ->
    case children(<<"channel">>, Root) of
        [Channel | _] -> feed(Channel, children(<<"item">>, Channel), rss, Url);
        _ -> {error, invalid_feed}
    end;
parse_root(<<"rdf">>, Root, Url) ->
    case children(<<"channel">>, Root) of
        [Channel | _] -> feed(Channel, children(<<"item">>, Root), rss, Url);
        _ -> {error, invalid_feed}
    end;
parse_root(<<"feed">>, Root, Url) -> feed(Root, children(<<"entry">>, Root), atom, Url);
parse_root(_, _, _) -> {error, not_feed}.

feed(Channel, Nodes, Kind, Url) ->
    Base = base(Channel, Url),
    Links = links(Channel, Base),
    Entries = [entry(N, Kind, Base, Url) || N <- lists:sublist(Nodes, ?MAX_ENTRIES)],
    IsZotonic = case {Kind, Entries} of
        {rss, _} -> false;
        {atom, []} -> undefined;
        {atom, _} ->
            case lists:any(fun(E) -> is_binary(maps:get(zotonic_export, E)) end, Entries) of
                true -> true;
                false when length(Nodes) > ?MAX_ENTRIES -> undefined;
                false -> false
            end
    end,
    {ok, #{url => Url, title => value(<<"title">>, Channel),
        website => case Kind of atom -> alternate(Links); rss -> absolute(value(<<"link">>, Channel), Base) end,
        hubs => [U || {<<"hub">>, U} <- Links],
        entries => Entries, is_zotonic_feed => IsZotonic,
        truncated => length(Nodes) > ?MAX_ENTRIES}}.

entry(Node, Kind, ParentBase, FeedUrl) ->
    Base = base(Node, ParentBase),
    Link = case Kind of
        atom -> alternate(links(Node, Base));
        rss -> absolute(value(<<"link">>, Node), Base)
    end,
    Guid = first([value(<<"id">>, Node), value(<<"guid">>, Node), attr(<<"rdf:about">>, Node)]),
    Body = first([value(<<"encoded">>, Node), content(Node), value(<<"description">>, Node)]),
    Tags = [first([attr(<<"term">>, N), text(N)]) || N <- children(<<"category">>, Node)],
    Enclosures = [absolute(first([attr(<<"url">>, N), attr(<<"href">>, N)]), Base)
        || N = {Name, _, _} <- descendants(Node),
           local(Name) =:= <<"enclosure">> orelse local(Name) =:= <<"thumbnail">>
           orelse (local(Name) =:= <<"content">> andalso attr(<<"url">>, N) =/= <<>>)
           orelse (local(Name) =:= <<"link">> andalso attr(<<"rel">>, N) =:= <<"enclosure">>)],
    {Images, Aliases} = image_variants(Body, Base),
    #{uri => identity(Guid, Link, FeedUrl), guid => Guid, url => Link,
        title => value(<<"title">>, Node), body => Body,
        summary => first([value(<<"summary">>, Node), value(<<"description">>, Node)]),
        author => first([value(<<"creator">>, Node), author(Node)]),
        published => date(first([value(<<"published">>, Node), value(<<"pubdate">>, Node), value(<<"date">>, Node)])),
        modified => date(value(<<"updated">>, Node)),
        tags => lists:usort([T || T <- Tags, T =/= <<>>]),
        media => unique([maps:get(U, Aliases, U) || U <- Enclosures ++ Images, U =/= <<>>]),
        media_aliases => Aliases,
        zotonic_export => zotonic_export(Kind, Node, Base),
        raw => Node}.

%% An explicit relation identifies a resource export; a generator name or JSON
%% alternate alone is not sufficient (WordPress also advertises JSON).
zotonic_export(atom, Node, Base) ->
    case [U || N <- children(<<"link">>, Node),
            attr(<<"rel">>, N) =:= ?ZOTONIC_RESOURCE_REL,
            attr(<<"type">>, N) =:= <<"application/json">>,
            U <- [absolute(attr(<<"href">>, N), Base)], U =/= <<>>] of
        [Url | _] -> Url;
        [] -> undefined
    end;
zotonic_export(_, _, _) -> undefined.

%% A publisher's absolute ID is the identity even when it is not a permalink.
%% Opaque GUIDs are scoped to the publisher origin, so category feeds overlap.
%% No identity is invented from a mutable title or an entry's position.
-spec identity(Guid, Link, FeedUrl) -> binary() | undefined when
    Guid :: binary(), Link :: binary(), FeedUrl :: binary().
identity(Guid, _, _) when byte_size(Guid) > ?MAX_IDENTITY_LENGTH -> undefined;
identity(<<>>, <<>>, _) -> undefined;
identity(<<>>, Link, _) -> Link;
identity(Guid, _, FeedUrl) ->
    case uri_string:parse(Guid) of
        #{scheme := _} -> Guid;
        _ ->
            #{scheme := S, host := H} = P = uri_string:parse(FeedUrl),
            Origin = uri_string:recompose((maps:with([port], P))#{scheme => S, host => H, path => <<>>}),
            Hash = binary:encode_hex(crypto:hash(sha256, <<Origin/binary, 0, Guid/binary>>), lowercase),
            <<"urn:zotonic:feed:", Hash/binary>>
    end.

-spec discover(Data, Url) -> {ok, [map()]} | {error, term()} when
    Data :: binary(), Url :: binary().
discover(Data, Url) -> discover(Data, Url, []).

-spec discover(Data, Url, Headers) -> {ok, [map()]} | {error, term()} when
    Data :: binary(), Url :: binary(), Headers :: list().
discover(Data, Url, Headers) ->
    case parse(Data, Url) of
        {ok, Feed} -> {ok, [maps:with([url, title], Feed)]};
        _ ->
            {ok, Metadata} = z_url_metadata:fetch_data(Url, Headers, <<>>),
            HeaderLinks = [A#{<<"href">> => absolute(maps:get(<<"href">>, A, <<>>), Url)}
                || A <- maps:get(<<"alternate">>, z_url_metadata:p(links, Metadata), [])],
            HtmlLinks = [(maps:from_list(A))#{<<"href">> => U}
                || {A, U} <- html_links(Data, Url),
                   lists:member(<<"alternate">>, binary:split(
                       proplists:get_value(<<"rel">>, A, <<>>), <<" ">>, [global]))],
            Found = [#{url => U, title => maps:get(<<"title">>, A, U)}
                || #{<<"href">> := U} = A <- HeaderLinks ++ HtmlLinks,
                   z_import_feed_http:url(U),
                   lists:member(maps:get(<<"type">>, A, <<>>),
                       [<<"application/rss+xml">>, <<"application/atom+xml">>, <<"application/rdf+xml">>])],
            case Found of [] -> {error, no_feeds}; _ -> {ok, lists:usort(Found)} end
    end.

-spec html_links(Data, Url) -> [tuple()] when Data :: binary(), Url :: binary().
html_links(Data, Url) ->
    case z_html_parse:parse(Data, #{mode => html, escape => false}) of
        {ok, Tree} ->
            Nodes = descendants(Tree),
            Base = first([absolute(attr(<<"href">>, N), Url) || N = {<<"base">>, _, _} <- Nodes] ++ [Url]),
            [{A, U} || {<<"link">>, A, _} <- Nodes,
                U <- [absolute(proplists:get_value(<<"href">>, A, <<>>), Base)], U =/= <<>>];
        _ -> []
    end.

-spec images(Html, Base) -> [binary()] when Html :: binary(), Base :: binary().
images(Html, Base) -> element(1, image_variants(Html, Base)).

%% Select a single largest declared variant per image. Keep all candidate URLs
%% as aliases so enclosures and the body's src/srcset reuse the same medium.
-spec image_variants(Html, Base) -> {[binary()], map()} when
    Html :: binary(), Base :: binary().
image_variants(Html, Base) ->
    case z_html_parse:parse(<<"<div>", Html/binary, "</div>">>, #{mode => html, escape => false}) of
        {ok, Tree} ->
            Groups = lists:foldl(fun(N, Acc) ->
                case image_candidates(N, Base) of
                    [] -> Acc;
                    Candidates -> merge_image_group(Candidates, Acc)
                end
            end, [], [N || N = {<<"img">>, _, _} <- descendants(Tree)]),
            {Urls, Aliases} = lists:foldl(fun(Candidates, {Us, As}) ->
                {_, Largest} = lists:max(Candidates),
                {[Largest | Us], lists:foldl(fun({_, U}, A) -> A#{U => Largest} end, As, Candidates)}
            end, {[], #{}}, Groups),
            {lists:reverse(Urls), Aliases};
        _ -> {[], #{}}
    end.

image_candidates(Node, Base) ->
    Src = absolute(first([attr(<<"data-src">>, Node), attr(<<"src">>, Node)]), Base),
    Default = case Src of <<>> -> []; _ -> [{{1, 1.0}, Src}] end,
    Set = first([attr(<<"data-srcset">>, Node), attr(<<"srcset">>, Node)]),
    Default ++ lists:filtermap(fun(Candidate) ->
        Parts = re:split(string:trim(Candidate), <<"\\s+">>, [{return, binary}]),
        case Parts of
            [U, Descriptor] -> srcset_candidate(U, Descriptor, Base);
            [U] -> srcset_candidate(U, <<"1x">>, Base);
            _ -> false
        end
    end, binary:split(Set, <<",">>, [global])).

srcset_candidate(U, Descriptor, Base) ->
    case {absolute(U, Base), re:run(Descriptor, <<"^([0-9]+(?:\\.[0-9]+)?)(w|x)$">>,
            [{capture, all_but_first, binary}])} of
        {<<>>, _} -> false;
        {Url, {match, [Number, Unit]}} ->
            Value = case binary:match(Number, <<".">>) of
                nomatch -> binary_to_integer(Number);
                _ -> binary_to_float(Number)
            end,
            case Value > 0 andalso (Unit =:= <<"x">> orelse is_integer(Value)) of
                true -> {true, {{case Unit of <<"w">> -> 2; <<"x">> -> 1 end, Value}, Url}};
                false -> false
            end;
        _ -> false
    end.

%% Repeated images sometimes expose different subsets of the same srcset.
%% Merge overlapping candidate sets before choosing, avoiding duplicate downloads.
merge_image_group(Candidates, Groups) ->
    URLs = [U || {_, U} <- Candidates],
    {Overlap, Other} = lists:partition(fun(G) ->
        lists:any(fun({_, U}) -> lists:member(U, URLs) end, G)
    end, Groups),
    case Overlap of
        [] -> Groups ++ [Candidates];
        _ -> merge_image_group(lists:usort(Candidates ++ lists:append(Overlap)), Other)
    end.

-spec date(Value) -> calendar:datetime() | undefined when Value :: binary().
date(<<>>) -> undefined;
date(Value) ->
    try
        case re:run(Value, <<"^\\d{4}-\\d\\d-\\d\\dT">>, [{capture, none}]) of
            match -> calendar:system_time_to_universal_time(
                calendar:rfc3339_to_system_time(binary_to_list(Value), [{unit, second}]), second);
            nomatch -> rss_date(Value)
        end
    of
        {{_, _, _}, {_, _, _}} = DT -> DT;
        _ -> undefined
    catch _:_ -> undefined end.

rss_date(Value) ->
    DT = httpd_util:convert_request_date(binary_to_list(Value)),
    case re:run(Value, <<"([+-])([0-9]{2})([0-9]{2})$">>, [{capture, all_but_first, binary}]) of
        {match, [Sign, H, M]} ->
            Offset = (binary_to_integer(H) * 60 + binary_to_integer(M)) * 60,
            Delta = case Sign of <<"+">> -> -Offset; _ -> Offset end,
            calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(DT) + Delta);
        nomatch -> DT
    end.

content(Node) ->
    case children(<<"content">>, Node) of
        [{_, A, C} = N | _] ->
            case proplists:get_value(<<"type">>, A) of
                <<"xhtml">> -> iolist_to_binary([z_html_parse:to_html(X) || X <- C]);
                _ -> text(N)
            end;
        _ -> <<>>
    end.

author(Node) ->
    case children(<<"author">>, Node) of
        [N | _] -> first([value(<<"name">>, N), text(N)]);
        _ -> <<>>
    end.
base(Node, Parent) -> first([absolute(attr(<<"xml:base">>, Node), Parent), Parent]).
absolute(U, B) -> z_import_feed_http:resolve(U, B).
local(Name) -> lists:last(binary:split(Name, <<":">>, [global])).
children(Name, {_, _, Cs}) -> [N || N = {Tag, _, _} <- Cs, local(Tag) =:= Name].
value(Name, Node) -> first([text(N) || N <- children(Name, Node)]).
attr(Name, {_, A, _}) -> proplists:get_value(Name, A, <<>>).
text({_, _, C}) -> iolist_to_binary([text(X) || X <- C]);
text(B) when is_binary(B) -> B;
text(_) -> <<>>.
first([]) -> <<>>;
first([<<>> | Rest]) -> first(Rest);
first([undefined | Rest]) -> first(Rest);
first([V | _]) -> V.
descendants({_, _, C} = N) -> [N | lists:append([descendants(X) || X <- C])];
descendants(_) -> [].
links(N, B) -> [{first([attr(<<"rel">>, L), <<"alternate">>]), absolute(attr(<<"href">>, L), B)}
    || L <- children(<<"link">>, N)].
alternate(Ls) -> first([U || {<<"alternate">>, U} <- Ls]).
unique(L) -> lists:reverse(element(2, lists:foldl(fun(U, {Seen, Acc}) ->
    case maps:is_key(U, Seen) of true -> {Seen, Acc}; false -> {Seen#{U => true}, [U | Acc]} end
end, {#{}, []}, L))).
