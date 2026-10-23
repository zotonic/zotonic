%% @copyright 2026 Marc Worrell
%% @doc RSS 2, RDF/RSS 1, Atom and JSON Feed normalization without creating XML-name atoms.
%% @end

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

-spec parse(Data, Url) -> {ok, map()} | {error, term()}
    when
        Data :: binary(),
        Url :: binary().
parse(Data, Url) when is_binary(Data), byte_size(Data) =< ?MAX_FEED_BYTES ->
    case trim_start(Data) of
        <<239, 187, 191, Rest/binary>> -> parse(Rest, Url);
        <<"{", _/binary>> = Json -> z_import_feed_json:parse(Json, Url, ?MAX_ENTRIES);
        _ -> parse_xml(Data, Url)
    end;
parse(_, _) ->
    {error, response_too_large}.

%% JSON whitespace is ASCII; do not decode untrusted input before the parser.
trim_start(<<C, Rest/binary>>) when C =:= 32; C =:= 9; C =:= 10; C =:= 13 -> trim_start(Rest);
trim_start(Data) -> Data.

parse_xml(Data, Url) ->
    %% No DTDs or custom entities, including external entities. z_html_parse
    %% retains names as binaries and does not fetch external documents.
    case re:run(Data, <<"<!\\s*(DOCTYPE|ENTITY)" >>, [caseless, {capture, none}]) of
        match ->
            {error, forbidden_xml_declaration};
        nomatch ->
            try z_html_parse:parse(Data, #{mode => xml, escape => false, lowercase => true}) of
                {ok, {Name, _, _} = Root} -> parse_root(local(Name), Root, Url);
                _ -> {error, invalid_feed}
            catch _:_ -> {error, invalid_feed} end
    end.

parse_root(<<"rss">>, Root, Url) ->
    case children(<<"channel">>, Root) of
        [Channel | _] -> feed(inherit_namespaces(Channel, Root), children(<<"item">>, Channel), rss, Url);
        _ -> {error, invalid_feed}
    end;
parse_root(<<"rdf">>, Root, Url) ->
    case children(<<"channel">>, Root) of
        [Channel | _] -> feed(inherit_namespaces(Channel, Root), children(<<"item">>, Root), rss, Url);
        _ -> {error, invalid_feed}
    end;
parse_root(<<"feed">>, Root, Url) ->
    feed(Root, children(<<"entry">>, Root), atom, Url);
parse_root(_, _, _) ->
    {error, not_feed}.

feed(Channel, Nodes, Kind, Url) ->
    Base = base(Channel, Url),
    Links = links(Channel, Base),
    Entries = [entry(ChildNode, Kind, Base, Url) || ChildNode <- lists:sublist(Nodes, ?MAX_ENTRIES)],
    IsZotonic = case {Kind, Entries} of
        {_, []} -> undefined;
        {_, _} ->
            case lists:any(fun(Entry) -> is_binary(maps:get(zotonic_export, Entry)) end, Entries) of
                true -> true;
                false when length(Nodes) > ?MAX_ENTRIES -> undefined;
                false -> false
            end
    end,
    {ok, #{
        url => Url,
        title => value(<<"title">>, Channel),
        website => case Kind of
                        atom -> alternate(Links);
                        rss -> absolute(value(<<"link">>, Channel), Base)
                   end,
        hubs => [LinkUrl || {<<"hub">>, LinkUrl} <- Links],
        entries => Entries,
        is_zotonic_feed => IsZotonic,
        refresh_seconds => refresh_seconds(Channel, Kind),
        truncated => length(Nodes) > ?MAX_ENTRIES
    }}.

%% Namespace declarations on rss/rdf are in scope on channel and its children.
inherit_namespaces({Name, Attrs, Nodes}, {_, ParentAttrs, _}) ->
    {Name, Attrs ++ [{K, V} || {<<"xmlns", _/binary>> = K, V} <- ParentAttrs,
                              not proplists:is_defined(K, Attrs)], Nodes}.

refresh_seconds({_, _, Nodes} = Channel, Kind) ->
    Ttl = case Kind of
        rss -> positive_seconds(first([text(Node) || {<<"ttl">>, _, _} = Node <- Nodes]), 60);
        atom -> 0
    end,
    FrequencyValue = syndication_value(<<"updatefrequency">>, Channel),
    Period = case syndication_value(<<"updateperiod">>, Channel) of
        <<>> when FrequencyValue =/= <<>> -> 86400;
        <<"hourly">> -> 3600;
        <<"daily">> -> 86400;
        <<"weekly">> -> 7 * 86400;
        <<"monthly">> -> 31 * 86400;
        <<"yearly">> -> 366 * 86400;
        _ -> 0
    end,
    Frequency = case FrequencyValue of
        <<>> -> 1;
        Value -> positive_seconds(Value, 1)
    end,
    Syndication = case Frequency of
        0 -> 0;
        _ -> (Period + Frequency - 1) div Frequency
    end,
    max(Ttl, Syndication).

syndication_value(Name, {_, Attrs, Nodes}) ->
    first([string:trim(text(Node)) || {Tag, ChildAttrs, _} = Node <- Nodes,
        [Prefix, Local] <- [binary:split(Tag, <<":">>)], Local =:= Name,
        proplists:get_value(<<"xmlns:", Prefix/binary>>, ChildAttrs ++ Attrs)
            =:= <<"http://purl.org/rss/1.0/modules/syndication/">>]).

positive_seconds(Value, Multiplier) when is_binary(Value) ->
    Trimmed = string:trim(Value),
    case re:run(Trimmed, <<"^[0-9]{1,10}$">>, [{capture, none}]) of
        match ->
            Seconds = binary_to_integer(Trimmed) * Multiplier,
            case Seconds > 0 andalso Seconds =< 2147483647 of
                true -> Seconds;
                false -> 0
            end;
        nomatch ->
            0
    end.

entry(Node, Kind, ParentBase, FeedUrl) ->
    Base = base(Node, ParentBase),
    Link = case Kind of
        atom -> alternate(links(Node, Base));
        rss -> absolute(value(<<"link">>, Node), Base)
    end,
    Guid = first([value(<<"id">>, Node), value(<<"guid">>, Node), attr(<<"rdf:about">>, Node)]),
    Body = first([value(<<"encoded">>, Node), content(Node), value(<<"description">>, Node)]),
    Tags = [first([attr(<<"term">>, ChildNode), text(ChildNode)]) || ChildNode <- children(<<"category">>, Node)],
    Enclosures = [absolute(first([attr(<<"url">>, ChildNode), attr(<<"href">>, ChildNode)]), Base)
        || ChildNode = {Name, _, _} <- descendants(Node),
           local(Name) =:= <<"enclosure">> orelse local(Name) =:= <<"thumbnail">>
           orelse (local(Name) =:= <<"content">> andalso attr(<<"url">>, ChildNode) =/= <<>>)
           orelse (local(Name) =:= <<"link">> andalso attr(<<"rel">>, ChildNode) =:= <<"enclosure">>)],
    {Images, Aliases} = image_variants(Body, Base),
    #{
        uri => identity(Guid, Link, FeedUrl),
        guid => Guid,
        url => Link,
        title => value(<<"title">>, Node),
        body => Body,
        summary => first([value(<<"summary">>, Node), value(<<"description">>, Node)]),
        author => first([value(<<"creator">>, Node), author(Node)]),
        published => date(first([value(<<"published">>, Node), value(<<"pubdate">>, Node), value(<<"date">>, Node)])),
        modified => date(value(<<"updated">>, Node)),
        tags => lists:usort([Tag || Tag <- Tags, Tag =/= <<>>]),
        media => unique([maps:get(LinkUrl, Aliases, LinkUrl) || LinkUrl <- Enclosures ++ Images, LinkUrl =/= <<>>]),
        media_aliases => Aliases,
        zotonic_export => zotonic_export(Kind, Node, Base),
        raw => Node
    }.

%% An explicit relation identifies a resource export; a generator name or JSON
%% alternate alone is not sufficient (WordPress also advertises JSON).
zotonic_export(Kind, Node, Base) when Kind =:= atom; Kind =:= rss ->
    case [LinkUrl || ChildNode <- children(<<"link">>, Node),
            attr(<<"rel">>, ChildNode) =:= ?ZOTONIC_RESOURCE_REL,
            attr(<<"type">>, ChildNode) =:= <<"application/json">>,
            LinkUrl <- [absolute(attr(<<"href">>, ChildNode), Base)], LinkUrl =/= <<>>] of
        [Url | _] -> Url;
        [] -> undefined
    end;
zotonic_export(_, _, _) ->
    undefined.

%% A publisher's absolute ID is the identity even when it is not a permalink.
%% Opaque GUIDs are scoped to the publisher origin, so category feeds overlap.
%% No identity is invented from a mutable title or an entry's position.
-spec identity(Guid, Link, FeedUrl) -> binary() | undefined
    when
        Guid :: binary(),
        Link :: binary(),
        FeedUrl :: binary().
identity(Guid, _, _) when byte_size(Guid) > ?MAX_IDENTITY_LENGTH -> undefined;
identity(<<>>, <<>>, _) -> undefined;
identity(<<>>, Link, _) -> Link;
identity(Guid, _, FeedUrl) ->
    case uri_string:parse(Guid) of
        #{scheme := _} ->
            Guid;
        _ ->
            #{scheme := Scheme, host := Host} = UrlParts = uri_string:parse(FeedUrl),
            Origin = uri_string:recompose((maps:with([port], UrlParts))#{scheme => Scheme, host => Host, path => <<>>}),
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
        {ok, Feed} ->
            {ok, [maps:with([url, title], Feed)]};
        _ ->
            {ok, Metadata} = z_url_metadata:fetch_data(Url, Headers, <<>>),
            HeaderLinks = [Attributes#{<<"href">> => absolute(maps:get(<<"href">>, Attributes, <<>>), Url)}
                || Attributes <- maps:get(<<"alternate">>, z_url_metadata:p(links, Metadata), [])],
            HtmlLinks = [(maps:from_list(Attributes))#{<<"href">> => LinkUrl}
                || {Attributes, LinkUrl} <- html_links(Data, Url),
                   lists:member(<<"alternate">>, binary:split(
                       proplists:get_value(<<"rel">>, Attributes, <<>>), <<" ">>, [global]))],
            Found = [#{url => LinkUrl, title => maps:get(<<"title">>, Attributes, LinkUrl)}
                || #{<<"href">> := LinkUrl} = Attributes <- HeaderLinks ++ HtmlLinks,
                   z_import_feed_http:url(LinkUrl),
                   lists:member(maps:get(<<"type">>, Attributes, <<>>),
                       [<<"application/rss+xml">>, <<"application/atom+xml">>, <<"application/rdf+xml">>, <<"application/feed+json">>])],
            case Found of [] -> {error, no_feeds}; _ -> {ok, lists:usort(Found)} end
    end.

-spec html_links(Data, Url) -> [tuple()] when Data :: binary(), Url :: binary().
html_links(Data, Url) ->
    case z_html_parse:parse(Data, #{mode => html, escape => false}) of
        {ok, Tree} ->
            Nodes = descendants(Tree),
            Base = first([absolute(attr(<<"href">>, ChildNode), Url) || ChildNode = {<<"base">>, _, _} <- Nodes] ++ [Url]),
            [{Attributes, LinkUrl} || {<<"link">>, Attributes, _} <- Nodes,
                LinkUrl <- [absolute(proplists:get_value(<<"href">>, Attributes, <<>>), Base)], LinkUrl =/= <<>>];
        _ ->
            []
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
            Groups = lists:foldl(fun(ChildNode, Acc) ->
                case image_candidates(ChildNode, Base) of
                    [] -> Acc;
                    Candidates -> merge_image_group(Candidates, Acc)
                end
            end, [], [ChildNode || ChildNode = {<<"img">>, _, _} <- descendants(Tree)]),
            {Urls, Aliases} = lists:foldl(
                fun(Candidates, {SelectedUrls, ImageAliases}) ->
                    {_, Largest} = lists:max(Candidates),
                    {[Largest | SelectedUrls],
                    lists:foldl(
                        fun({_, LinkUrl}, AliasMap) -> AliasMap#{LinkUrl => Largest} end,
                        ImageAliases,
                        Candidates)}
                end,
                {[], #{}},
                Groups),
            {lists:reverse(Urls), Aliases};
        _ ->
            {[], #{}}
    end.

image_candidates(Node, Base) ->
    Src = z_import_feed_media:resolve(first([attr(<<"data-src">>, Node), attr(<<"src">>, Node)]), Base),
    Default = case Src of
        <<>> -> [];
        _ -> [{{1, 1.0}, Src}]
    end,
    Set = first([attr(<<"data-srcset">>, Node), attr(<<"srcset">>, Node)]),
    Default ++ lists:filtermap(
        fun(Candidate) ->
            Parts = re:split(string:trim(Candidate), <<"\\s+">>, [{return, binary}]),
            case Parts of
                [LinkUrl, Descriptor] -> srcset_candidate(LinkUrl, Descriptor, Base);
                [LinkUrl] -> srcset_candidate(LinkUrl, <<"1x">>, Base);
                _ -> false
            end
        end,
        srcset_entries(Set)).

%% A data URL contains a comma itself. Split on candidate separators after
%% descriptors, or on trailing commas after a descriptorless URL token.
srcset_entries(Set) ->
    srcset_entries(string:trim(Set, leading, " \t\r\n,"), []).

srcset_entries(<<>>, Acc) -> lists:reverse(Acc);
srcset_entries(Set, Acc) ->
    Pattern = case Set of <<"data:", _/binary>> -> <<"^\\S+">>; _ -> <<"^[^\\s,]+">> end,
    {match, [Url]} = re:run(Set, Pattern, [{capture, first, binary}]),
    Rest = binary:part(Set, byte_size(Url), byte_size(Set) - byte_size(Url)),
    {Candidate, Tail} = case binary:last(Url) of
        $, ->
            {string:trim(Url, trailing, ","), Rest};
        _ ->
            case binary:split(Rest, <<",">>) of
                [Descriptor, More] -> {<<Url/binary, Descriptor/binary>>, More};
                [Descriptor] -> {<<Url/binary, Descriptor/binary>>, <<>>}
            end
    end,
    srcset_entries(string:trim(Tail, leading, " \t\r\n,"), [Candidate | Acc]).

srcset_candidate(LinkUrl, Descriptor, Base) ->
    case {
            z_import_feed_media:resolve(LinkUrl, Base),
            re:run(Descriptor, <<"^([0-9]+(?:\\.[0-9]+)?)(w|x)$">>, [{capture, all_but_first, binary}])
        }
    of
        {<<>>, _} ->
            false;
        {Url, {match, [Number, Unit]}} ->
            Value = case binary:match(Number, <<".">>) of
                nomatch -> binary_to_integer(Number);
                _ -> binary_to_float(Number)
            end,
            case Value > 0 andalso (Unit =:= <<"x">> orelse is_integer(Value)) of
                true -> {true, {{case Unit of <<"w">> -> 2; <<"x">> -> 1 end, Value}, Url}};
                false -> false
            end;
        _ ->
            false
    end.

%% Repeated images sometimes expose different subsets of the same srcset.
%% Merge overlapping candidate sets before choosing, avoiding duplicate downloads.
merge_image_group(Candidates, Groups) ->
    URLs = [LinkUrl || {_, LinkUrl} <- Candidates],
    {Overlap, Other} = lists:partition(
        fun(Group) ->
            lists:any(fun({_, LinkUrl}) -> lists:member(LinkUrl, URLs) end, Group)
        end, Groups),
    case Overlap of
        [] -> Groups ++ [Candidates];
        _ -> merge_image_group(lists:usort(Candidates ++ lists:append(Overlap)), Other)
    end.

-spec date(Value) -> calendar:datetime() | undefined when Value :: binary().
date(<<>>) ->
    undefined;
date(Value) ->
    try
        case re:run(Value, <<"^\\d{4}-\\d\\d-\\d\\dT">>, [{capture, none}]) of
            match ->
                calendar:system_time_to_universal_time(
                    calendar:rfc3339_to_system_time(binary_to_list(Value), [{unit, second}]),
                    second);
            nomatch ->
                rss_date(Value)
        end
    of
        {{_, _, _}, {_, _, _}} = DateTime -> DateTime;
        _ -> undefined
    catch
        _:_ -> undefined
    end.

rss_date(Value) ->
    DateTime = httpd_util:convert_request_date(binary_to_list(Value)),
    case re:run(Value, <<"([+-])([0-9]{2})([0-9]{2})$">>, [{capture, all_but_first, binary}]) of
        {match, [Sign, Hours, Minutes]} ->
            Offset = (binary_to_integer(Hours) * 60 + binary_to_integer(Minutes)) * 60,
            Delta = case Sign of
                <<"+">> -> -Offset;
                _ -> Offset
            end,
            calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(DateTime) + Delta);
        nomatch ->
            DateTime
    end.

content(Node) ->
    case children(<<"content">>, Node) of
        [{_, Attributes, Children} = ChildNode | _] ->
            case proplists:get_value(<<"type">>, Attributes) of
                <<"xhtml">> -> iolist_to_binary([z_html_parse:to_html(Child) || Child <- Children]);
                _ -> text(ChildNode)
            end;
        _ ->
            <<>>
    end.

author(Node) ->
    case children(<<"author">>, Node) of
        [ChildNode | _] -> first([value(<<"name">>, ChildNode), text(ChildNode)]);
        _ -> <<>>
    end.

base(Node, Parent) ->
    first([absolute(attr(<<"xml:base">>, Node), Parent), Parent]).

absolute(LinkUrl, BaseUrl) ->
    z_import_feed_http:resolve(LinkUrl, BaseUrl).

local(Name) ->
    lists:last(binary:split(Name, <<":">>, [global])).

children(Name, {_, _, Children}) ->
    [ChildNode || ChildNode = {Tag, _, _} <- Children, local(Tag) =:= Name].

value(Name, Node) ->
    first([text(ChildNode) || ChildNode <- children(Name, Node)]).

attr(Name, {_, Attributes, _}) ->
    proplists:get_value(Name, Attributes, <<>>).

text({_, _, Children}) ->
    iolist_to_binary([text(Child) || Child <- Children]);
text(Text) when is_binary(Text) ->
    Text;
text(_) ->
    <<>>.

first([]) -> <<>>;
first([<<>> | Rest]) -> first(Rest);
first([undefined | Rest]) -> first(Rest);
first([Value | _]) -> Value.

descendants({_, _, Children} = ChildNode) ->
    [ChildNode | lists:append([descendants(Child) || Child <- Children])];
descendants(_) ->
    [].

links(Node, BaseUrl) ->
    [
        {first([attr(<<"rel">>, LinkNode), <<"alternate">>]), absolute(attr(<<"href">>, LinkNode), BaseUrl)}
        || LinkNode <- children(<<"link">>, Node)
    ].

alternate(Links) ->
    first([LinkUrl || {<<"alternate">>, LinkUrl} <- Links]).

unique(Urls) ->
    {_Seen, UniqueUrls} = lists:foldl(
        fun(Url, {Seen, Acc}) ->
            case maps:is_key(Url, Seen) of
                true -> {Seen, Acc};
                false -> {Seen#{Url => true}, [Url | Acc]}
            end
        end,
        {#{}, []},
        Urls),
    lists:reverse(UniqueUrls).
