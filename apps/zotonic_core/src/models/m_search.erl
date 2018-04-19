%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-04-15
%% @doc Search model, used as an interface to the search functions of modules etc.

%% Copyright 2009 Marc Worrell
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

%% A search question is represented by:
%%      {search_name, [PropList]}
%% The search result is represented by:
%%      {search_result, [Results], [PropList], PagingInfo}
%% The search options are always sorted before the search is done.
%%
%% {% for id in m.search[{featured cat="accessoiries"}] %}
%%
%% Paging is done by fetching the first ?SEARCH_LIMIT rows and then return a slice from those rows.
%% This result set should be cached for a short while (depending on writes by
%% the user_id associated with the visitor).

-module(m_search).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/2,

    search/2,
    search_pager/2,
    get_result/3
]).

-include_lib("zotonic.hrl").

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), z:context() ) -> {term(), list()}.
m_get([ paged, SearchProps | Rest ], Context) ->
    {search_pager(SearchProps, Context), Rest};
m_get([ SearchProps | Rest ], Context) ->
    {search(SearchProps, Context), Rest};
m_get(Vs, _Context) ->
    lager:error("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {undefined, []}.


%% @doc Perform a search, wrap the result in a m_search_result record
%% @spec search(Search, Context) -> #m_search_result{}
search({SearchName, Props}, Context) ->
    {Page, PageLen, Props1} = get_optional_paging_props(Props),
    try
        Result = z_search:search({SearchName, Props1}, {(Page - 1) * PageLen + 1, PageLen}, Context),
        Total1 = case Result#search_result.total of
            undefined -> length(Result#search_result.result);
            Total -> Total
        end,
        #m_search_result{result=Result, total=Total1, search_name=SearchName, search_props=Props}
    catch
        throw:Error ->
            lager:error("Error in m.search[~p] error: ~p",
                        [{SearchName, Props}, Error]),
            empty_result(SearchName, Props, PageLen)
    end;
search(SearchName, Context) ->
    search({z_convert:to_atom(SearchName), []}, Context).


%% @doc Perform a paged search, wrap the result in a m_search_result record
%% @spec search_pager(Search, Context) -> #m_search_result{}
search_pager({SearchName, Props}, Context) ->
    {Page, PageLen, Props1} = get_paging_props(Props),
    try
        Result = z_search:search_pager({SearchName, Props1}, Page, PageLen, Context),
        Total1 = case Result#search_result.total of
            undefined -> length(Result#search_result.result);
            Total -> Total
        end,
        #m_search_result{result=Result, total=Total1, search_name=SearchName, search_props=Props1}
    catch
        throw:Error ->
            lager:error("Error in m.search[~p] error: ~p",
                        [{SearchName, Props}, Error]),
            empty_result(SearchName, Props, PageLen)
    end;
search_pager(SearchName, Context) ->
    search_pager({z_convert:to_atom(SearchName), []}, Context).

empty_result(SearchName, Props, PageLen) ->
    #m_search_result{
        result=#search_result{
            result=[],
            page=1,
            pagelen=PageLen,
            total=0,
            all=[],
            pages=1
        },
        total=0,
        search_name=SearchName,
        search_props=Props
    }.


get_result(N, #m_search_result{result=Result}, _Context) when is_integer(N) ->
    try
        lists:nth(N, Result#search_result.result)
    catch
        _:_ -> undefined
    end;
get_result(result, #m_search_result{result=Result}, _Context) ->
    Result#search_result.result;
get_result(name, Result, _Context) ->
    Result#m_search_result.search_name;
get_result(props, Result, _Context) ->
    Result#m_search_result.search_props;
get_result(total, Result, _Context) ->
    Result#m_search_result.total;
get_result(facets, Result, _Context) ->
    #search_result{facets = Facets} = Result#m_search_result.result,
    Facets;
get_result(pages, Result, _Context) ->
    case Result#m_search_result.result of
        #search_result{pages=Pages} -> Pages;
        undefined -> Result#m_search_result.pages
    end;
get_result(page, Result, _Context) ->
    case Result#m_search_result.result of
        #search_result{page=Page} -> Page;
        undefined -> Result#m_search_result.page
    end;
get_result(_Key, _Result, _Context) ->
    undefined.



get_optional_paging_props(Props) ->
    case proplists:is_defined(page, Props) orelse proplists:is_defined(pagelen, Props) of
        true -> get_paging_props(Props);
        false -> {1, 1000, lists:keysort(1, Props)}
    end.

get_paging_props(Props) ->
    Page = case proplists:get_value(page, Props) of
        undefined -> 1;
        PageProp -> try z_convert:to_integer(PageProp) catch _:_ -> 1 end
    end,
    PageLen = case proplists:get_value(pagelen, Props) of
        undefined -> ?SEARCH_PAGELEN;
        PageLenProp -> try z_convert:to_integer(PageLenProp) catch _:_ -> ?SEARCH_PAGELEN end
    end,
    P1 = proplists:delete(page, Props),
    P2 = proplists:delete(pagelen, P1),
    {Page, PageLen, lists:keysort(1, P2)}.
