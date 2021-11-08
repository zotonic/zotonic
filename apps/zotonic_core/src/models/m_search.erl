%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2021 Marc Worrell
%% @doc Search model, used as an interface to the search functions of modules etc.

%% Copyright 2009-2021 Marc Worrell
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

%% A deprecated search question is represented by:
%%      {search_name, [PropList]}
%% The search result is represented by:
%%      {search_result, [Results], [PropList], PagingInfo}
%%
%% {% for id in m.search.featured::%{ cat: "accessoiries" } %}
%%

-module(m_search).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/3,

    search/2,
    search_pager/2,

    search/3
]).

-include_lib("zotonic.hrl").

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"paged">>, SearchName | Rest ], Msg, Context) when is_binary(SearchName) ->
    case search(SearchName, search_args(Msg), Context) of
        {error, _} = Error ->
            Error;
        {ok, Result} ->
            {ok, {Result, Rest}}
    end;
m_get([ <<"paged">>, {Name, Props} = SearchProps | Rest ], _Msg, Context) when is_list(Props), is_atom(Name) ->
    case search_deprecated(SearchProps, true, Context) of
        {error, _} = Error ->
            Error;
        {ok, Result} ->
            {ok, {Result, Rest}}
    end;
m_get([ SearchName | Rest ], Msg, Context) when is_binary(SearchName) ->
    case search(SearchName, search_args(Msg), Context) of
        {error, _} = Error ->
            Error;
        {ok, Result} ->
            {ok, {Result, Rest}}
    end;
m_get([ {Name, Props} = SearchProps | Rest ], _Msg, Context) when is_list(Props), is_atom(Name) ->
    case search_deprecated(SearchProps, false, Context) of
        {error, _} = Error ->
            Error;
        {ok, Result} ->
            {ok, {Result, Rest}}
    end;
m_get([ <<"paged">> ], Msg, Context) ->
    Args = search_args(Msg),
    case search(<<"query">>, Args#{ <<"qargs">> => true }, Context) of
        {error, _} = Error ->
            Error;
        {ok, Result} ->
            {ok, {Result, []}}
    end;
m_get([], Msg, Context) ->
    Args = search_args(Msg),
    case search(<<"query">>, Args#{ <<"qargs">> => true }, Context) of
        {error, _} = Error ->
            Error;
        {ok, Result} ->
            {ok, {Result, []}}
    end.


%% @doc Perform a search. Pass page and pagelen as arguments for paging.
-spec search( binary(), map(), z:context() ) -> {ok, #m_search_result{}} | {error, term()}.
search(Name, Args, Context) when is_binary(Name), is_map(Args) ->
    {Page, PageLen, Args1} = get_paging_props(Args, Context),
    try
        Result = z_search:search(Name, Args1, Page, PageLen, Context),
        Total = Result#search_result.total,
        {ok, #m_search_result{result=Result, total=Total, search_name=Name, search_args=Args}}
    catch
        throw:Error ->
            lager:error("Error in m.search[~p] error: ~p", [{Name, Args}, Error]),
            {error, Error}
    end.

%% @deprecated Use m_search:search/3
search({Name, Args}, Context) when is_binary(Name), is_map(Args) ->
    search(Name, Args, Context);
search({Name, Args}, Context) when is_binary(Name), is_list(Args) ->
    Args1 = z_search:props_to_map(Args),
    search(Name, Args1, Context);
search(Search, Context) ->
    case search_deprecated(Search, false, Context) of
        {ok, Result} ->
            Result;
        {error, _} ->
            empty_result()
    end.

%% @deprecated Use m_search:search/3
search_pager(Search, Context) ->
    case search(Search, true, Context) of
        {ok, Result} ->
            Result;
        {error, _} ->
            empty_result()
    end.

search_args(#{ payload := Args }) when is_map(Args) ->
    Args;
search_args(_) ->
    #{}.


% Deprecated interface.
search_deprecated({Name, Props}, _IsPaged = true, Context) when is_atom(Name), is_list(Props) ->
    {Page, PageLen, Props1} = get_paging_props(Props, Context),
    try
        Result = z_search:search_pager({Name, Props1}, Page, PageLen, Context),
        Total = Result#search_result.total,
        {ok, #m_search_result{result=Result, total=Total, search_name=Name, search_args=Props1}}
    catch
        throw:Error ->
            lager:error("Error in m.search[~p] error: ~p", [{Name, Props}, Error]),
            {error, Error}
    end;
search_deprecated({Name, Props}, _IsPaged = false, Context) when is_atom(Name), is_list(Props) ->
    {Page, PageLen, Props1} = get_optional_paging_props(Props, Context),
    try
        Offset = (Page - 1) * PageLen + 1,
        Result = z_search:search({Name, Props1}, {Offset, PageLen}, Context),
        Total1 = case Result#search_result.total of
            undefined -> length(Result#search_result.result);
            Total -> Total
        end,
        {ok, #m_search_result{result=Result, total=Total1, search_name=Name, search_args=Props}}
    catch
        throw:Error ->
            lager:error("Error in m.search[~p] error: ~p", [{Name, Props}, Error]),
            {error, Error}
    end.

% try_rsc_search(SearchName, IsPaged, Context) ->
%     case m_rsc:rid(SearchName, Context) of
%         undefined ->
%             {error, enoent};
%         RscId ->
%             search({'query', [ {query_id, RscId} ]}, IsPaged, Context)
%     end.

empty_result() ->
    #m_search_result{
        result = #search_result{
            result = [],
            page = 1,
            pagelen = ?SEARCH_PAGELEN,
            total = 0,
            all = [],
            pages = 1
        },
        total = 0,
        search_name = <<"error">>,
        search_args = #{}
    }.


get_optional_paging_props(Props, Context) when is_list(Props) ->
    % Deprecated proplists handling
    case proplists:is_defined(page, Props) orelse proplists:is_defined(pagelen, Props) of
        true -> get_paging_props(Props, Context);
        false -> {1, ?SEARCH_PAGELEN, Props}
    end.

get_paging_props(undefined, _Context) ->
    {1, ?SEARCH_PAGELEN, #{}};
get_paging_props(#{ <<"qargs">> := true } = Args, Context) ->
    try
        Page = case z_convert:to_integer(z_context:get_q(<<"page">>, Context)) of
            undefined ->
                case maps:get(<<"page">>, Args, 1) of
                    undefined -> 1;
                    P -> z_convert:to_integer(P)
                end;
            P -> P
        end,
        PageLen = case z_convert:to_integer(z_context:get_q(<<"page">>, Context)) of
            undefined ->
                case maps:get(<<"pagelen">>, Args, ?SEARCH_PAGELEN) of
                    undefined -> ?SEARCH_PAGELEN;
                    PL -> z_convert:to_integer(PL)
                end;
            PL -> PL
        end,
        {Page, PageLen, maps:without([ <<"page">>, <<"pagelen">> ], Args)}
    catch
        _:_ ->
            {1, ?SEARCH_PAGELEN, maps:without([ <<"page">>, <<"pagelen">> ], Args)}
    end;
get_paging_props(Args, _Context) when is_map(Args) ->
    Page = case maps:get(<<"page">>, Args, 1) of
        undefined -> 1;
        P -> try z_convert:to_integer(P) catch _:_ -> 1 end
    end,
    PageLen = case maps:get(<<"pagelen">>, Args, 1) of
        undefined -> ?SEARCH_PAGELEN;
        PL -> try z_convert:to_integer(PL) catch _:_ -> ?SEARCH_PAGELEN end
    end,
    {Page, PageLen, maps:without([ <<"page">>, <<"pagelen">> ], Args)};
get_paging_props(Props, _Context) when is_list(Props) ->
    % Deprecated proplists handling
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
    {Page, PageLen, P2}.
