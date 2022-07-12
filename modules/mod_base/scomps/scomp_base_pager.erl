%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2015 Marc Worrell
%% Date: 2009-04-18
%% @doc Show the pager for the search result

%% Copyright 2009-2015 Marc Worrell
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
-behaviour(gen_scomp).

-export([vary/2, render/3]).
-export([test/0]).

-include("zotonic.hrl").

% Pages before/after the current page
-define(DELTA, 2).
-define(SLIDE, (?DELTA + ?DELTA + 1)).


vary(_Params, _Context) -> nocache.


render(Params, _Vars, Context) ->
    Result       = proplists:get_value(result, Params),
    Dispatch     = case proplists:get_value(dispatch, Params) of
                        undefined -> z_context:get(zotonic_dispatch, Context, search);
                        Dp -> Dp
                   end,

    HideSinglePage  = z_convert:to_bool(proplists:get_value(hide_single_page, Params, false)),
    CleanedArgs  = proplists:delete(dispatch, proplists:delete(result, proplists:delete(hide_single_page, Params))),

    DispatchArgs = case proplists:is_defined(qargs, CleanedArgs) of
        true -> CleanedArgs;
        false -> [{qargs,true}|CleanedArgs]
    end,

    Result1 = case Result of
        #m{model=m_search, value=MResult} -> MResult;
        _ -> Result
    end,

    case Result1 of
        #m_search_result{result=[]} ->
            {ok, <<>>};
        #m_search_result{result=undefined} ->
            {ok, <<>>};
        #m_search_result{result=#search_result{pages=0}} ->
            {ok, <<>>};
        #m_search_result{result=#search_result{page=Page, pages=1}} ->
            {ok, build_html(Page, 1, false, HideSinglePage, Dispatch, DispatchArgs, Context)};
        #m_search_result{result=#search_result{page=Page, pages=Pages, is_total_estimated=IsEstimated}} ->
            Html = build_html(Page, Pages, IsEstimated, HideSinglePage, Dispatch, DispatchArgs, Context),
            {ok, Html};
        #search_result{result=[]} ->
            {ok, <<>>};
        #search_result{pages=undefined} ->
            {ok, <<>>};
        #search_result{page=Page, pages=Pages, is_total_estimated=IsEstimated} ->
            Html = build_html(Page, Pages, IsEstimated, HideSinglePage, Dispatch, DispatchArgs, Context),
            {ok, Html};
        [ Chunk | _ ] = List when is_list(Chunk) ->
            % Paginated list with page chunks
            Page = lookup_arg(page, 1, Params, Context),
            Pages = length(List),
            {ok, build_html(Page, Pages, false, HideSinglePage, Dispatch, DispatchArgs, Context)};
        List when is_list(List) ->
            % Flat list
            render_list(List, Params, HideSinglePage, Dispatch, DispatchArgs, Context);
        #rsc_list{list=Ids} ->
            render_list(Ids, Params, HideSinglePage, Dispatch, DispatchArgs, Context);
        _ ->
            {error, <<"scomp_pager: search result is not a #search_result{} or list">>}
    end.

render_list([], _Params, _HideSinglePage, _Dispatch, _DispatchArgs, _Context) ->
    {ok, ""};
render_list(List, Params, HideSinglePage, Dispatch, DispatchArgs, Context) ->
    PageLen = lookup_arg(pagelen, ?SEARCH_PAGELEN, Params, Context),
    Page = lookup_arg(page, 1, Params, Context),
    Pages = (length(List) - 1) div PageLen + 1,
    {ok, build_html(Page, Pages, false, HideSinglePage, Dispatch, DispatchArgs, Context)}.

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

build_html(_Page, Pages, _IsEtimated, true, _Dispatch, _DispatchArgs, _Context) when Pages =< 1 ->
    <<>>;
build_html(Page, Pages, IsEstimated, _HideSinglePage, Dispatch, DispatchArgs, Context) ->
    {S,M,E} = pages(Page, Pages),
    Urls = urls(S, M, E, IsEstimated, Dispatch, DispatchArgs, Context),
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
        {dispatch, Dispatch},
        {is_estimated, IsEstimated}
    ],
    {Html, _} = z_template:render_to_iolist("_pager.tpl", Props, Context),
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
    Start1 = [ {N, z_dispatcher:url_for(Dispatch, [{page,N}|DispatchArgs], Context)} || N <- Start ],
    Slider1 = [ {N, z_dispatcher:url_for(Dispatch, [{page,N}|DispatchArgs], Context)} || N <- Slider ],
    End1 = [ {N, z_dispatcher:url_for(Dispatch, [{page,N}|DispatchArgs], Context)} || N <- End ],
    case {Start1, Slider1, End1} of
        {[], S, []} -> S;
        {[], S, [_]} when IsEstimated -> S ++ [ {undefined, sep} ];
        {[], S, E} -> S ++ [ {undefined, sep} | E ];
        {B, S, []} -> B ++ [ {undefined, sep} | S ];
        {B, S, [_]} when IsEstimated -> B ++ [ {undefined, sep} | S ] ++ [ {undefined, sep} ];
        {B, S, E} -> B ++ [ {undefined, sep} | S ] ++ [ {undefined, sep} | E ]
    end.

seq(A,B) when B < A -> [];
seq(A,B) -> lists:seq(A,B).


test() ->
    C = z_context:new(default),
    R = #search_result{result=[a], pages=100, page=10},
    {ok, H} = render([{result,R}], [], C),
    list_to_binary(H).

