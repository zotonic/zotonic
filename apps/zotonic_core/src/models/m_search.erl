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
    m_get/3,

    search/2,
    search_pager/2,
    get_result/3
]).

-include_lib("zotonic.hrl").

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"paged">>, SearchName | Rest ], _Msg, Context) when is_binary(SearchName) ->
    case search(SearchName, true, Context) of
        {error, _} = Error ->
            Error;
        {ok, Result} ->
            {ok, {Result, Rest}}
    end;
m_get([ <<"paged">>, {Name, Props} = SearchProps | Rest ], _Msg, Context) when is_list(Props), is_atom(Name) ->
    case search(SearchProps, true, Context) of
        {error, _} = Error ->
            Error;
        {ok, Result} ->
            {ok, {Result, Rest}}
    end;
m_get([ SearchName | Rest ], _Msg, Context) when is_binary(SearchName) ->
    case search(SearchName, false, Context) of
        {error, _} = Error ->
            Error;
        {ok, Result} ->
            {ok, {Result, Rest}}
    end;
m_get([ {Name, Props} = SearchProps | Rest ], _Msg, Context) when is_list(Props), is_atom(Name) ->
    case search(SearchProps, false, Context) of
        {error, _} = Error ->
            Error;
        {ok, Result} ->
            {ok, {Result, Rest}}
    end;
m_get([ <<"paged">> ], _Msg, Context) ->
    case search({'query', [ {qargs, true} ]}, true, Context) of
        {error, _} = Error ->
            Error;
        {ok, Result} ->
            {ok, {Result, []}}
    end;
m_get([], _Msg, Context) ->
    case search({'query', [ {qargs, true} ]}, false, Context) of
        {error, _} = Error ->
            Error;
        {ok, Result} ->
            {ok, {Result, []}}
    end.

search(Search, Context) ->
    case search(Search, false, Context) of
        {ok, Result} ->
            Result;
        {error, _} ->
            empty_result()
    end.

search_pager(Search, Context) ->
    case search(Search, true, Context) of
        {ok, Result} ->
            Result;
        {error, _} ->
            empty_result()
    end.


%% @doc Perform a search, wrap the result in a m_search_result record
search({SearchName, Props}, true, Context) when is_atom(SearchName), is_list(Props) ->
    {Page, PageLen, Props1} = get_paging_props(Props),
    try
        Result = z_search:search_pager({SearchName, Props1}, Page, PageLen, Context),
        Total = Result#search_result.total,
        {ok, #m_search_result{result=Result, total=Total, search_name=SearchName, search_props=Props1}}
    catch
        throw:Error ->
            lager:error("Error in m.search[~p] error: ~p",
                        [{SearchName, Props}, Error]),
            {error, Error}
    end;
search({SearchName, Props}, false, Context) when is_atom(SearchName), is_list(Props) ->
    {Page, PageLen, Props1} = get_optional_paging_props(Props),
    try
        Result = z_search:search({SearchName, Props1}, {(Page - 1) * PageLen + 1, PageLen}, Context),
        Total1 = case Result#search_result.total of
            undefined -> length(Result#search_result.result);
            Total -> Total
        end,
        {ok, #m_search_result{result=Result, total=Total1, search_name=SearchName, search_props=Props}}
    catch
        throw:Error ->
            lager:error("Error in m.search[~p] error: ~p",
                        [{SearchName, Props}, Error]),
            {error, Error}
    end;
search(SearchName, IsPaged, Context) when is_atom(SearchName) ->
    search({SearchName, []}, IsPaged, Context);
search(SearchName, IsPaged, Context) when is_binary(SearchName) ->
    case to_atom(SearchName) of
        {ok, Atom} ->
            case search({Atom, []}, IsPaged, Context) of
                {ok, _} = Result ->
                    Result;
                {error, _} ->
                    try_rsc_search(SearchName, IsPaged, Context)
            end;
        error ->
            try_rsc_search(SearchName, IsPaged, Context)
    end.

try_rsc_search(SearchName, IsPaged, Context) ->
    case m_rsc:rid(SearchName, Context) of
        undefined ->
            {error, enoent};
        RscId ->
            search({'query', [ {query_id, RscId} ]}, IsPaged, Context)
    end.


to_atom(N) ->
    try
        {ok, binary_to_existing_atom(N, utf8)}
    catch
        _:_ -> error
    end.

empty_result() ->
    #m_search_result{
        result=#search_result{
            result = [],
            page = 1,
            pagelen = 10,
            total = 0,
            all = [],
            pages = 1
        },
        total = 0,
        search_name = error,
        search_props = []
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
