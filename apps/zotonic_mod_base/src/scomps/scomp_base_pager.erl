%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2023 Marc Worrell
%% @doc Show the pager for the search result
%% @end

%% Copyright 2009-2023 Marc Worrell
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

-module(scomp_base_pager).
-moduledoc("
Show a pager for search results.

This generates a pager as seen on the search results pages. It is used in conjunction with a paged search result.

For example, a fulltext search where the search parameters come from the query string:


```django
{% with m.search.paged[{fulltext cat=q.qcat text=q.qs page=q.page pagelen=20}] as result %}
  <ul>
    {% pager result=result dispatch=\"admin_overview_rsc\" qargs %}
    {% for id,score in result %}
      <li><a href=\"\">{{ m.rsc[id].title }}</a></li>
    {% empty %}
      <li>Nothing found</li>
    {% endfor %}
  </ul>
{% endwith %}
```

This will show a list of titles and above that the links to the next, previous and other pages.

Note

that we are using `m.search.paged` here and not [m.search](/id/doc_model_model_search). The pager only works with
results from `m.search.paged`.

The generated pager code will look something like (when searching for the text “filter”):


```django
<ul class=\"pager block\">
<li>« prev</li>
<li class=\"current\"><a href=\"/admin/overview?qs=filter&page=1\">1</a></li>
<li><a href=\"/admin/overview?qs=filter&page=2\">2</a></li>
<li><a href=\"/admin/overview?qs=filter&page=3\">3</a></li>
<li><a href=\"/admin/overview?qs=filter&page=4\">4</a></li>
<li class=\"pager-sep\">…</li>
<li><a href=\"/admin/overview?qs=filter&page=5\">5</a></li>
<li><a href=\"/admin/overview?qs=filter&page=2\">next»</a></li>
</ul>
```

The pager tag accepts the following arguments:

| Argument             | Description                                                                      | Example                           |
| -------------------- | -------------------------------------------------------------------------------- | --------------------------------- |
| result               | The result from a search. This must be a `#search_result` or a list. This mostly the result of a `m.search` call. | result=mysearchresult             |
| dispatch             | Name of the dispatch rule to be used for the page urls. Defaults to the dispatch rule of the current page. If there is no dispatch rule defined then `none` is used and only a link with the query arguments is generated. For example `?page=2&qs=test` | dispatch=”searchresult”           |
| qargs                | Append all the arguments in the HTTP request’s query string whose name starts with a ‘q’ as an argument to the dispatch rule. | qargs                             |
| hash                 | Hash to append to the pagination links. Used to jump to the search results on the load of a new page. | hash=”#content-pager”             |
| hide\\\\_single\\\\_page | When this argument is true, do not show the pager when the result fits on one page (e.g. the pager will be useless). | hide\\\\_single\\\\_page=1            |
| template             | Name of the template for rendering the pager. Defaults to `_pager.tpl`. See below for specific arguments passed. | template=”\\\\_pager.tpl”           |
| page                 | Current page number, the first page is page number 1. Fetched from the search result, this argument, or `q.page`. | page=2                            |
| pagelen              | Number of items per page, fetch from the search result or `q.pagelen`. Defaults to 20. | pagelen=10                        |
| topic                | The topic for handling the link clicks. Normally a link click will just load a new page in the browser. This intercepts the click and sends the new link to the given topic. | topic=”/model/location/post/push” |
| \\\\*                  | Any other argument is used as an argument for the dispatch rule.                 |                                   |



Pager template
--------------

The pager is rendered using a template. The default template for the pager is `_pager.tpl`.

The pager template receives the following variables:

*   `prev_url` The url to the previous page, `undefined` if at first page.
*   `next_url` The url to the next page, `undefined` if at last page.
*   `pages` A list of tuples. Either `{PageNumber, Url}` or `{undefined, sep}` (*sep* is an atom).
*   `page` The current page number
*   All other arguments passed to the scomp (**attention:** these are also used as dispatch arguments)

The default `_pager.tpl` displays an ellipsis for the `sep` entry.

If the result set is empty then the template is not rendered. The template is also not rendered if there is a single
page *and* `hide_single_page` is set.



Result argument
---------------

It is also possible to pass a list, a `#rsc_list{ list=Ids }` record, or a list of lists (pages) for the resut. In this
case you need to perform the pagination for displaying the results yourself. You can use the
[chunk](/id/doc_template_filter_filter_chunk) for this. The pager scomp will fetch the `page` and `pagelen` from the
pager arguments, or from the query arguments (if any). If the list is pre-chunked then the pages does not need the
`pagelen` argument.



Topic
-----

If the search result is refreshed on the page without a page reload, then the `topic` should be passed. This is the
topic that directly or indirectly triggers a refresh of the element displaying the search result.

For example, you can make a live search:


```django
{% live topic=\"model/location/event/qlist\"
        template=\"_search.tpl\"
        method=\"patch\"
%}
```

With a `_search.tpl` template like this:


```django
<form data-onsubmit-topic=\"model/location/post/qlist/submit\"
      data-oninput-topic=\"model/location/post/qlist/submit\"
      method=\"GET\"
>
    <input type=\"text\" name=\"qs\" value=\"{{ q.qs|escape }}\">
</form>

{% with m.search.query::%{ qargs: true, page: q.page } as result %}
  <ul>
    {% for id in result %}
      <a href=\"{{ id.page_url }}\">{{ id.title }}</a>
    {% endfor %}
  </ul>
  {% pager result=result topic=\"model/location/post/push\" qargs %}
{% endwith %}
```

Typing in the search will automatically update the search result without reloading the page, as will a click on one of
the pager links.
").
-behaviour(zotonic_scomp).

-export([vary/2, render/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

% Pages before/after the current page
-define(DELTA, 2).

vary(_Params, _Context) -> nocache.


render(Params, _Vars, Context) ->
    Result = proplists:get_value(result, Params),
    Dispatch = case proplists:get_value(dispatch, Params) of
                   undefined -> z_context:get(zotonic_dispatch, Context, none);
                   Dp -> Dp
               end,
    HideSinglePage  = z_convert:to_bool(proplists:get_value(hide_single_page, Params, false)),
    Template = proplists:get_value(template, Params, "_pager.tpl"),
    DispatchArgs  = lists:foldl(
        fun(Arg, Acc) ->
            proplists:delete(Arg, Acc)
        end,
        Params,
        [dispatch, result, hide_single_page, template, topic, hash]),
    TemplateVars = [
        {topic, proplists:get_value(topic, Params)},
        {hash, proplists:get_value(hash, Params)}
    ],
    case Result of
        #search_result{page=Page, pages=undefined, prev=Prev, next=Next} ->
            Html = build_prevnext(Template, Page, Prev, Next, Dispatch, DispatchArgs, TemplateVars, Context),
            {ok, Html};
        #search_result{page=Page, pages=Pages, is_total_estimated=IsEstimated} when Page =< Pages ->
            Html = build_html(Template, Page, Pages, IsEstimated, HideSinglePage, Dispatch, DispatchArgs, TemplateVars, Context),
            {ok, Html};
        #search_result{page=Page, pages=Pages} when Page > Pages ->
            Html = build_html(Template, 2, 1, false, false, Dispatch, DispatchArgs, TemplateVars, Context),
            {ok, Html};
        [ Chunk | _ ] = List when is_list(Chunk) ->
            % Paginated list with page chunks
            Page = lookup_arg(page, 1, Params, Context),
            Pages = length(List),
            {ok, build_html(Template, Page, Pages, false, HideSinglePage, Dispatch, DispatchArgs, TemplateVars, Context)};
        List when is_list(List) ->
            % Flat list
            render_list(Template, List, Params, HideSinglePage, Dispatch, DispatchArgs, TemplateVars, Context);
        #rsc_list{list=Ids} ->
            render_list(Template, Ids, Params, HideSinglePage, Dispatch, DispatchArgs, TemplateVars, Context);
        undefined ->
            render_list(Template, [], Params, HideSinglePage, Dispatch, DispatchArgs, TemplateVars, Context);
        _ ->
            ?DEBUG(Result),
            {error, <<"scomp_pager: search result is not a #search_result{} or list">>}
    end.

render_list(_Template, [], _Params, _HideSinglePage, _Dispatch, _DispatchArgs, _TemplateVars, _Context) ->
    {ok, <<>>};
render_list(Template, List, Params, HideSinglePage, Dispatch, DispatchArgs, TemplateVars, Context) ->
    PageLen = lookup_arg(pagelen, ?SEARCH_PAGELEN, Params, Context),
    Page = lookup_arg(page, 1, Params, Context),
    Pages = (length(List) - 1) div PageLen + 1,
    {ok, build_html(Template, Page, Pages, false, HideSinglePage, Dispatch, DispatchArgs, TemplateVars, Context)}.

lookup_arg(Name, Default, Params, Context) ->
    V = case proplists:get_value(Name, Params) of
        undefined -> undefined;
        P -> try z_convert:to_integer(P) catch _:_ -> undefined end
    end,
    V1 = case V of
        undefined ->
            case z_context:get_q(Name, Context) of
                undefined -> undefined;
                Q -> try z_convert:to_integer(Q) catch _:_ -> undefined end
            end;
        _ ->
            V
    end,
    case V1 of
        undefined -> Default;
        N when N =< 0 -> Default;
        _ -> V1
    end.

build_prevnext(_Template, 1, _Prev, false, _Dispatch, _DispatchArgs, _TemplateVars, _Context) ->
    <<>>;
build_prevnext(Template, Page, Prev, Next, Dispatch, DispatchArgs, TemplateVars, Context) ->
    DispatchQArgs = append_qargs(DispatchArgs, Context),
    Props = [
        {prev_url, case Page =< 1 of
                        true -> undefined;
                        false ->  url_for(Dispatch, [{page,Prev}|DispatchQArgs], Context)
                   end},
        {next_url, case Next of
                        false -> undefined;
                        _ ->  url_for(Dispatch, [{page,Next}|DispatchQArgs], Context)
                   end},
        {pages, []},
        {page, Page},
        {dispatch, Dispatch},
        {is_estimated, true}
        | TemplateVars
    ],
    {Html, _} = z_template:render_to_iolist(Template, Props, Context),
    Html.

build_html(_Template, _Page, Pages, _IsEstimated, true, _Dispatch, _DispatchArgs, _TemplateVars, _Context) when Pages =< 1 ->
    <<>>;
build_html(Template, Page, Pages, IsEstimated, _HideSinglePage, Dispatch, DispatchArgs, TemplateVars, Context) ->
    {S,M,E} = pages(Page, Pages),
    DispatchQArgs = append_qargs(DispatchArgs, Context),
    Urls = urls(S, M, E, IsEstimated, Dispatch, DispatchQArgs, Context),
    Props = [
        {prev_url, case Page =< 1 of
                        true -> undefined;
                        false ->  url_for(Dispatch, [{page,Page-1}|DispatchQArgs], Context)
                   end},
        {next_url, case Page >= Pages of
                        true -> undefined;
                        false ->  url_for(Dispatch, [{page,Page+1}|DispatchQArgs], Context)
                   end},
        {pages, Urls},
        {page, Page},
        {dispatch, Dispatch},
        {is_estimated, IsEstimated}
        | DispatchArgs
    ] ++ TemplateVars,
    {Html, _} = z_template:render_to_iolist(Template, Props, Context),
    Html.

url_for(page, Args, Context) ->
    case proplists:get_value(id, Args) of
        undefined ->
            z_dispatcher:url_for(page, Args, Context);
        Id ->
            Url = m_rsc:p(Id, page_url, Context),
            ArgsEncoded = encode_args(proplists:delete(slug, proplists:delete(id, Args))),
            case binary:match(Url, <<"?">>) of
                {_,_} ->
                    iolist_to_binary([ Url, $&, ArgsEncoded ]);
                nomatch ->
                    iolist_to_binary([ Url, $?, ArgsEncoded ])
            end
    end;
url_for(Dispatch, Args, Context) ->
    z_dispatcher:url_for(Dispatch, Args, Context).


%% @doc Append all query arguments iff they are not mentioned in the arglist and if qargs parameter is set.
append_qargs(Args, Context) ->
    case proplists:get_value(qargs, Args) of
        undefined ->
            Args;
        false ->
            proplists:delete(qargs, Args);
        true ->
            Args1 = proplists:delete(qargs, Args),
            merge_qargs(z_context:get_qargs(Context), Args1);
        L when is_list(L) ->
            Args1 = proplists:delete(qargs, Args),
            merge_qargs(L, Args1);
        M when is_map(M) ->
            Args1 = proplists:delete(qargs, Args),
            merge_qargs(maps:to_list(M), Args1)
    end.

merge_qargs([], Args) ->
    Args;
merge_qargs(Qs, Args) ->
    Ks = [ z_convert:to_binary(A) || {A, _} <- Args ],
    lists:foldr(
        fun({A, _} = AV, Acc) ->
            case lists:member(A, Ks) of
                true ->
                    Acc;
                false ->
                    [ AV | Acc ]
            end
        end,
        Args,
        Qs).

encode_args(Args) ->
    lists:join($&, [
        [z_url:url_encode(K), $=, z_url:url_encode(V)] || {K,V} <- Args
    ]).


pages(Page, Pages) ->
    SliderMin = erlang:max(1, Page - ?DELTA),
    SliderMax = if
        Page < ?DELTA -> erlang:min(Pages, ?DELTA + ?DELTA);
        true -> erlang:min(Pages, Page + ?DELTA)
    end,
    Slider = seq(SliderMin, SliderMax),
    {Start, Slider1} = if
        SliderMin =:= 1 -> {[], Slider};
        SliderMin =:= 2 -> {[], [1|Slider]};
        SliderMin =:= 3 -> {[], [1,2|Slider]};
        true -> {[1], Slider}
    end,
    {End, Slider2} = if
        SliderMax =:= Pages -> {[], Slider1};
        SliderMax =:= Pages - 1 -> {[], Slider1 ++ [Pages]};
        SliderMax =:= Pages - 2 -> {[], Slider1 ++ [Pages-1,Pages]};
        true -> {[Pages], Slider1}
    end,
    {Start1, Slider3} = case {Slider2,End} of
        {[N|_], []} ->
            Extra = ?DELTA + ?DELTA - length(Slider2),
            if
                N - Extra =< 3 ->
                    {[], seq(1,N-1) ++ Slider2};
                true ->
                    {Start, seq(N-Extra, N-1) ++ Slider2}
            end;
        {_, _} ->
            {Start, Slider2}
    end,
    {Start1, Slider3, End}.

urls(Start, Slider, End, IsEstimated, Dispatch, DispatchArgs, Context) ->
    Start1 = [ {N, url_for(Dispatch, [{page,N}|DispatchArgs], Context)} || N <- Start ],
    BeforeSlider =
        case Slider of
            [] ->
                [];
            [N1Slider|_] ->
                [ {undefined, url_for(Dispatch, [{page,N}|DispatchArgs], Context)} || N <- [N1Slider-1] ]
        end,
    Slider1 = [ {N, url_for(Dispatch, [{page,N}|DispatchArgs], Context)} || N <- Slider ],
    AfterSlider =
        case Slider of
            [] ->
                [];
            [_|_] ->
                [ {undefined, url_for(Dispatch, [{page,N}|DispatchArgs], Context)} || N <- [lists:last(Slider)+1] ]
        end,
    End1 = [ {N, url_for(Dispatch, [{page,N}|DispatchArgs], Context)} || N <- End ],

    case {Start1, Slider1, End1} of
        {[], S, []} -> S;
        {[], S, [_]} when IsEstimated -> S ++ AfterSlider;
        {[], S, E} -> S ++ AfterSlider ++ E;
        {B, S, []} -> B ++ BeforeSlider ++ S;
        {B, S, [_]} when IsEstimated -> B ++ BeforeSlider ++ S ++ AfterSlider;
        {B, S, E} -> B ++ BeforeSlider ++ S ++ AfterSlider ++ E
    end.

seq(A,B) when B < A -> [];
seq(A,B) -> lists:seq(A,B).


% test() ->
%     C = z_context:new(default),
%     R = #search_result{result=[a], pages=100, page=10},
%     {ok, H} = render([{result,R}], [], C),
%     list_to_binary(H).

