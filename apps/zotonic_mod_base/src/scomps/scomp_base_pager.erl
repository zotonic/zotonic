%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2017 Marc Worrell
%% @doc Show the pager for the search result

%% Copyright 2009-2017 Marc Worrell
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
-behaviour(zotonic_scomp).

-export([vary/2, render/3]).
-export([test/0]).

-include_lib("zotonic_core/include/zotonic.hrl").

% Pages before/after the current page
-define(DELTA, 2).
-define(SLIDE, (?DELTA + ?DELTA + 1)).


vary(_Params, _Context) -> nocache.


render(Params, _Vars, Context) ->
    Result = proplists:get_value(result, Params),
    Dispatch = case proplists:get_value(dispatch, Params) of
                   undefined -> z_context:get(zotonic_dispatch, Context, search);
                   Dp -> Dp
               end,
    HideSinglePage = proplists:get_value(hide_single_page, Params),
    Template = proplists:get_value(template, Params, "_pager.tpl"),
    DispatchArgs  = lists:foldl(
        fun(Arg, Acc) ->
            proplists:delete(Arg, Acc)
        end,
        Params,
        [dispatch, result, hide_single_page, template]),

    case Result of
        #m_search_result{result=[]} ->
            {ok, ""};
        #m_search_result{result=undefined} ->
            {ok, ""};
        #m_search_result{result=#search_result{pages=0}} ->
            {ok, ""};
        #m_search_result{result=#search_result{page=Page, pages=1}} ->
            case z_convert:to_bool(HideSinglePage) of
                true ->
                    {ok, []};
                false ->
                    {ok, build_html(Template, Page, 1, Dispatch, DispatchArgs, Context)}
            end;
        #m_search_result{result=#search_result{page=Page, pages=Pages}} ->
            Html = build_html(Template, Page, Pages, Dispatch, DispatchArgs, Context),
            {ok, Html};
        #search_result{result=[]} ->
            {ok, ""};
        #search_result{pages=undefined} ->
            {ok, ""};
        #search_result{page=Page, pages=Pages} ->
            Html = build_html(Template, Page, Pages, Dispatch, DispatchArgs, Context),
            {ok, Html};
        _ ->
            {error, "scomp_pager: search result is not a #search_result{}"}
    end.


build_html(Template, Page, Pages, Dispatch, DispatchArgs, Context) ->
    {S,M,E} = pages(Page, Pages),
    Urls = urls(S, M, E, Dispatch, DispatchArgs, Context),
    Props = [
        {prev_url, case Page =< 1 of
                        true -> undefined;
                        false ->  url_for(Dispatch, [{page,Page-1}|DispatchArgs], Context)
                   end},
        {next_url, case Page >= Pages of
                        true -> undefined;
                        false ->  url_for(Dispatch, [{page,Page+1}|DispatchArgs], Context)
                   end},
        {pages, Urls},
        {page, Page},
        {dispatch, Dispatch}
        | DispatchArgs
    ],
    {Html, _} = z_template:render_to_iolist(Template, Props, Context),
    Html.

url_for(page, Args, Context) ->
    case proplists:get_value(id, Args) of
        undefined ->
            z_dispatcher:url_for(page, Args, Context);
        Id ->
            Url = m_rsc:p(Id, page_url, Context),
            ArgsWithQArgs = append_qargs(Args, Context),
            ArgsEncoded = encode_args(proplists:delete(slug, proplists:delete(id, ArgsWithQArgs))),
            case binary:match(Url, <<"?">>) of
                {_,_} ->
                    iolist_to_binary([ Url, $&, ArgsEncoded ]);
                nomatch ->
                    iolist_to_binary([ Url, $?, ArgsEncoded ])
            end
    end;
url_for(Dispatch, Args, Context) ->
    z_dispatcher:url_for(Dispatch, Args, Context).


%% @doc Append all query arguments iff they are not mentioned in the arglist and if qargs parameter is set
%% Taken from z_dispatcher.erl
append_qargs(Args, Context) ->
    case proplists:get_value(qargs, Args) of
        undefined ->
            Args;
        false ->
            proplists:delete(qargs, Args);
        true ->
            Args1 = proplists:delete(qargs, Args),
            Qs = z_context:get_q_all(Context),
            lists:foldr(fun
                            ({[$q|_]=Key,_Value}=A, Acc) ->
                                case proplists:is_defined(Key, Args) of
                                    true -> Acc;
                                    false -> [A|Acc]
                                end;
                            (_, Acc) ->
                                Acc
                        end,
                        Args1,
                        Qs)
    end.

encode_args(Args) ->
    z_utils:combine($&, [
        [z_url:url_encode(K), $=, z_url:url_encode(V)] || {K,V} <- Args
    ]).

pages(Page, Pages) ->
    AtStart = (not (Page == Pages)) and (Page < ?SLIDE),
    AtEnd = (not AtStart) and (Page > (Pages - (?SLIDE - 1))),
    Start = case AtStart of
        true ->
            % Together "1 .. "
            seq(1, erlang:min(?SLIDE, Pages - 1));
        false ->
            % Separate "1 ... 3"
            [1]
    end,
    End = case AtEnd of
        true ->
            % Together "10 .. 15"
            seq(erlang:max(2, Pages - ?SLIDE + 1), Pages);
        false ->
            [Pages]
    end,
    Middle = case (not AtStart) and (not AtEnd) of
        true ->
            seq(erlang:max(2, Page - ?DELTA), erlang:min(Pages - 1, Page + ?DELTA));
        false ->
            []
    end,
    {Start, Middle, End}.


urls(Start, Middle, End, Dispatch, DispatchArgs, Context) ->
    UrlStart  = [ {N, z_dispatcher:url_for(Dispatch, [{page,N}|DispatchArgs], Context)} || N <- Start ],
    UrlMiddle = [ {N, z_dispatcher:url_for(Dispatch, [{page,N}|DispatchArgs], Context)} || N <- Middle ],
    UrlEnd    = [ {N, z_dispatcher:url_for(Dispatch, [{page,N}|DispatchArgs], Context)} || N <- End ],
    {Part1,Next} = case Middle of
        [] ->
            {UrlStart, max(Start) + 1};
        [N|_] when N == 2 ->
            % Now Start is always of the format [1]
            {UrlStart ++ UrlMiddle, lists:max(Middle) + 1};
        _ ->
            {UrlStart ++ [{undefined, sep}|UrlMiddle], lists:max(Middle) + 1}
    end,
    case End of
        [] ->
            Part1;
        [M|_] ->
            if
                M == Next -> Part1 ++ UrlEnd;
                true -> Part1 ++ [{undefined, sep}|UrlEnd]
            end
    end.


max([]) -> 0;
max(L) -> lists:max(L).

seq(A,B) when B < A -> [];
seq(A,B) -> lists:seq(A,B).


test() ->
    C = z_context:new(default),
    R = #search_result{result=[a], pages=100, page=10},
    {ok, H} = render([{result,R}], [], C),
    list_to_binary(H).

