%% @copyright 2026 Marc Worrell
%% @doc Shared feed URLs and resource-to-JSON Feed conversion.
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

-module(export_feed).

-export([url/4, json_header/4, json_item/2]).

-spec url(Dispatch, Id, Format, Context) -> binary() when
    Dispatch :: atom(), Id :: term(), Format :: atom(), Context :: z:context().
url(Dispatch, Id, Format, Context) when Dispatch =:= export_rsc; Dispatch =:= export_rsc_query ->
    z_dispatcher:url_for(Dispatch, [{id, Id}, {type, Format}, {absolute_url, true}], none, Context);
url(_Dispatch, _Id, _Format, Context) ->
    z_context:abs_url(z_context:get_req_path(Context), Context).

-spec json_header(Id, Dispatch, Header, Context) -> map() when
    Id :: term(), Dispatch :: atom(), Header :: term(), Context :: z:context().
json_header(Id, Dispatch, Header, Context) ->
    SiteTitle = plain(m_site:title(Context), Context),
    Title = plain(
        case Header of
            undefined -> prop(Id, title, Context);
            _ -> Header
        end, Context),
    #{
        <<"version">> => <<"https://jsonfeed.org/version/1.1">>,
        <<"title">> => case Title of <<>> -> SiteTitle; _ -> <<SiteTitle/binary, " - ", Title/binary>> end,
        <<"home_page_url">> => z_dispatcher:url_for(home, [{absolute_url, true}], none, Context),
        <<"feed_url">> => url(Dispatch, Id, jsonfeed, Context),
        <<"language">> => z_convert:to_binary(z_context:language(Context))
    }.

-spec json_item(Id, Context) -> {ok, map()} | skip when
    Id :: term(), Context :: z:context().
json_item(Id, Context) when is_integer(Id) ->
    case m_rsc:is_visible(Id, Context) of
        true -> item(Id, Context);
        false -> skip
    end;
json_item(Row, Context) when is_map(Row) ->
    case z_convert:to_bool(prop(Row, is_visible, Context)) of
        true -> item(Row, Context);
        false -> skip
    end;
json_item(_, _) ->
    skip.

item(Id, Context) ->
    Uri = prop(Id, uri, Context),
    case is_binary(Uri) andalso Uri =/= <<>> of
        false -> skip;
        true ->
            Body = translated(prop(Id, body, Context), Context),
            Summary = plain(prop(Id, summary, Context), Context),
            Content = case Body of
                <<>> -> #{<<"content_text">> => Summary};
                _ -> #{<<"content_html">> => Body}
            end,
            Item = compact(Content#{
                <<"id">> => Uri,
                <<"url">> => url_value(prop(Id, page_url_abs, Context)),
                <<"title">> => plain(prop(Id, title, Context), Context),
                <<"summary">> => Summary,
                <<"date_published">> => filter_date:date(prop(Id, publication_start, Context), <<"c">>, true, Context),
                <<"date_modified">> => filter_date:date(prop(Id, modified, Context), <<"c">>, true, Context),
                <<"attachments">> => attachments(prop(Id, medium, Context), Context),
                <<"authors">> => authors(Id, Context)
            }),
            % Custom map rows need not represent a local resource export endpoint.
            Native = case Id of
                N when is_integer(N) -> #{<<"_zotonic">> => #{<<"resource_url">> =>
                    z_dispatcher:url_for(api_rsc_export, [{id, N}, {absolute_url, true}], none, Context)}};
                _ -> #{}
            end,
            {ok, maps:merge(Item, Native)}
    end.

authors(Id, Context) ->
    Authors = case Id of
        N when is_integer(N) ->
            m_rsc:o(N, author, Context);
        _ ->
            case prop(Id, o, Context) of
                #{<<"author">> := As} when is_list(As) -> As;
                _ -> []
            end
    end,
    Author = case Authors of
        [A | _] -> A;
        _ -> prop(Id, creator_id, Context)
    end,
    case Author of
        undefined -> [];
        _ ->
            {Name, _} = z_template:render_to_iolist("_name.tpl", [{id, Author}], Context),
            case plain(iolist_to_binary(Name), Context) of
                <<>> -> [];
                Text -> [compact(#{<<"name">> => Text, <<"url">> => url_value(prop(Author, uri, Context))})]
            end
    end.

attachments(#{<<"filename">> := Filename, <<"mime">> := Mime} = Medium, Context)
        when is_binary(Filename), Filename =/= <<>>, is_binary(Mime), Mime =/= <<>> ->
    [
        compact(#{
            <<"url">> => z_dispatcher:url_for(media_attachment, [{star, Filename}, {absolute_url, true}], none, Context),
            <<"mime_type">> => Mime,
            <<"size_in_bytes">> => maps:get(<<"size">>, Medium, undefined)
        })
    ];
attachments(_, _) ->
    [].

prop(undefined, _, _) ->
    undefined;
prop(Row, Key, _) when is_map(Row) ->
    maps:get(atom_to_binary(Key), Row, maps:get(Key, Row, undefined));
prop(Id, Key, Context) ->
    m_rsc:p(Id, Key, Context).

translated(undefined, _) ->
    <<>>;
translated(Value, Context) ->
    z_convert:to_binary(z_trans:lookup_fallback(Value, Context)).

plain(Value, Context) ->
    z_html:unescape(z_html:strip(translated(Value, Context))).

compact(Map) ->
    maps:filter(fun(_, Value) -> Value =/= undefined andalso Value =/= [] end, Map).

url_value(undefined) ->
    undefined;
url_value(Url) ->
    z_html:unescape(z_convert:to_binary(Url)).
