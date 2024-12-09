%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2024 Marc Worrell
%% @doc Search model, used as an interface to the search functions of modules etc.
%% @end

%% Copyright 2009-2024 Marc Worrell
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
m_get([ <<"count">>, SearchName | Rest ], Msg, Context) when is_binary(SearchName) ->
    case search(SearchName, search_args(Msg), #{ is_count_rows => true }, Context) of
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
m_get([ {Name, Props} = SearchProps | Rest ], _Msg, Context) when is_list(Props), is_atom(Name) ->
    case search_deprecated(SearchProps, false, Context) of
        {error, _} = Error ->
            Error;
        {ok, Result} ->
            {ok, {Result, Rest}}
    end;
m_get([ <<"paged">> ], Msg, Context) ->
    case search(<<"query">>, search_args(Msg), Context) of
        {error, _} = Error ->
            Error;
        {ok, Result} ->
            {ok, {Result, []}}
    end;
m_get([ <<"count">> ], Msg, Context) ->
    case search(<<"query">>, search_args(Msg), #{ is_count_rows => true }, Context) of
        {error, _} = Error ->
            Error;
        {ok, Result} ->
            {ok, {Result, []}}
    end;
m_get([ SearchName | Rest ], Msg, Context) when is_binary(SearchName) ->
    case search(SearchName, search_args(Msg), Context) of
        {error, _} = Error ->
            Error;
        {ok, Result} ->
            {ok, {Result, Rest}}
    end;
m_get([], Msg, Context) ->
    case search(<<"query">>, search_args(Msg), Context) of
        {error, _} = Error ->
            Error;
        {ok, Result} ->
            {ok, {Result, []}}
    end.


%% @doc Perform a search. Pass page and pagelen as arguments for paging.
-spec search( binary(), map(), z:context() ) -> {ok, #search_result{}} | {error, term()}.
search(Name, Args, Context) when is_binary(Name), is_map(Args) ->
    search(Name, Args, #{}, Context).

-spec search( binary(), map(), ForcedOptions, z:context() ) -> {ok, #search_result{}} | {error, term()} when
    ForcedOptions :: z_search:search_options().
search(Name, Args, ForcedOptions, Context) when is_binary(Name), is_map(Args), is_map(ForcedOptions) ->
    {Page, PageLen, Args1} = get_paging_props(Args, Context),
    {Options, Args2} = get_search_options(Args1),
    Options1 = maps:merge(Options, ForcedOptions),
    try
        {ok, z_search:search(Name, Args2, Page, PageLen, Options1, Context)}
    catch
        Result:Reason:Stack ->
            ?LOG_ERROR(#{
                text => <<"Error in m.search">>,
                in => zotonic_core,
                result => case Result of
                    throw -> error;
                    _ -> Result
                end,
                reason => Reason,
                search_name => Name,
                search_args => Args,
                stack => Stack
            }),
            {error, Reason}
    end.

%% @deprecated Use m_search:search/3
-spec search(S, Context) -> #search_result{}
    when S :: {atom(), proplists:proplist()}
            | {binary(), map()}
            | {binary(), proplists:proplist()},
         Context :: z:context().
search({Name, Args}, Context) when is_binary(Name), is_map(Args) ->
    case search(Name, Args, Context) of
        {ok, S} -> S;
        {error, _} -> empty_result()
    end;
search({Name, Args}, Context) when is_binary(Name), is_list(Args) ->
    Args1 = z_search:props_to_map(Args),
    case search(Name, Args1, Context) of
        {ok, S} -> S;
        {error, _} -> empty_result()
    end;
search(Search, Context) ->
    case search_deprecated(Search, false, Context) of
        {ok, Result} ->
            Result;
        {error, _} ->
            empty_result()
    end.

%% @deprecated Use m_search:search/3
-spec search_pager(S, Context) -> #search_result{}
    when S :: {atom(), proplists:proplist()}
            | {binary(), map()}
            | {binary(), proplists:proplist()},
         Context :: z:context().
search_pager(Search, Context) ->
    case search_deprecated(Search, true, Context) of
        {ok, Result} ->
            Result;
        {error, _} ->
            empty_result()
    end.

search_args(#{ payload := Args }) when is_map(Args) ->
    Args;
search_args(#{ payload := [ [_,_] | _ ] = Args }) ->
    Args;
search_args(_) ->
    #{ <<"qargs">> => true }.


% Deprecated interface.
search_deprecated({Name, Props}, _IsPaged = true, Context) when is_atom(Name), is_list(Props) ->
    {Page, PageLen, Props1} = get_paging_props(Props, Context),
    try
        {ok, z_search:search_pager({Name, Props1}, Page, PageLen, Context)}
    catch
        throw:Error ->
            ?LOG_ERROR(#{
                text => <<"Error in m.search">>,
                in => zotonic_core,
                result => error,
                reason => Error,
                search_name => Name,
                search_args => Props
            }),
            {error, Error}
    end;
search_deprecated({Name, Props}, _IsPaged = false, Context) when is_atom(Name), is_list(Props) ->
    {Page, PageLen, Props1} = get_optional_paging_props(Props, Context),
    try
        Offset = (Page - 1) * PageLen + 1,
        Result = z_search:search({Name, Props1}, {Offset, PageLen}, Context),
        Result1 = case Result#search_result.total of
            undefined -> Result#search_result{ total = length(Result#search_result.result) };
            _ -> Result
        end,
        {ok, Result1}
    catch
        throw:Error ->
            ?LOG_ERROR(#{
                text => <<"Error in m.search">>,
                in => zotonic_core,
                result => error,
                reason => Error,
                search_name => Name,
                search_args => Props
            }),
            {error, Error}
    end.

empty_result() ->
    #search_result{
        search_name = <<"error">>,
        search_args = #{},
        result = [],
        options = #{},
        page = 1,
        pagelen = ?SEARCH_PAGELEN,
        total = 0,
        is_total_estimated = false,
        pages = 1
    }.


get_search_options(#{ <<"options">> := Options } = Args) when is_map(Options) ->
    Options1 = z_search:map_to_options(Options),
    {Options1, maps:remove(<<"options">>, Args)};
get_search_options(Args) ->
    {#{}, Args}.

get_optional_paging_props(Props, Context) when is_list(Props) ->
    % Deprecated proplists handling
    case proplists:is_defined(page, Props) orelse proplists:is_defined(pagelen, Props) of
        true -> get_paging_props(Props, Context);
        false -> {1, ?SEARCH_PAGELEN, Props}
    end.

get_paging_props(#{ <<"qargs">> := true } = Args, Context) ->
    try
        Page = case z_convert:to_integer(maps:get(<<"page">>, Args, undefined)) of
            undefined ->
                case z_convert:to_integer(z_context:get_q(<<"page">>, Context)) of
                    undefined -> 1;
                    P -> z_convert:to_integer(P)
                end;
            P ->
                P
        end,
        PageLen = case z_convert:to_integer(maps:get(<<"pagelen">>, Args, undefined)) of
            undefined ->
                case z_convert:to_integer(z_context:get_q(<<"pagelen">>, Context)) of
                    undefined -> ?SEARCH_PAGELEN;
                    PL -> z_convert:to_integer(PL)
                end;
            PL ->
                PL
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
    PageLen = case maps:get(<<"pagelen">>, Args, ?SEARCH_PAGELEN) of
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

