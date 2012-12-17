%% @author Andreas Stenius <git@astekk.se>
%% @copyright 2012 Andreas Stenius
%% @doc 'sort' filter, sort a resource id list on property.

%% Copyright 2012 Andreas Stenius
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

-module(filter_sort).
-export([sort/2, sort/3]).

-include_lib("zotonic.hrl").

%% no arg, sort on id, default order
sort(Input, _Context) when is_list(Input) ->
    lists:sort(Input);
sort(Input, Context) when is_record(Input, rsc_list) ->
    sort(Input, [id], Context).

sort(Input, Arg, _Context) when is_list(Input) ->
    case is_opt(Arg) of 
        false -> Input;
        ascending -> lists:sort(Input);
        descending -> lists:reverse(lists:sort(Input))
    end;
sort(#rsc_list{ list=Rscs }, Args, Context) when is_list(Args) ->
    Args1 = case z_string:is_string(Args) of
                true -> [Args];
                false -> Args
            end,
    sort_list({0, Rscs}, {ascending, Args1}, Context);
sort(Input, Arg, Context) when is_record(Input, rsc_list) ->
    sort(Input, [Arg], Context).


%% Internal functions

sort_list({_, Result}, {_, []}, _Context) ->
    #rsc_list{ list=lists:flatten(Result) };
sort_list(Input, {DefaultOpt, Args}, Context) ->
    {Opt, [Prop|Rest]} = split_opt(Args, DefaultOpt),
    Output = do_sort(Input, Opt, Prop, Context),
    sort_list(Output, {Opt, Rest}, Context).

do_sort({0, Rscs}, Opt, Prop, Context) ->
    Tagged = [{get_prop(Id, Prop, Context), Id} || Id <- Rscs],
    Sorted = lists:keysort(1, Tagged),
    Fold = case Opt of
               ascending -> fun lists:foldr/3;
               descending -> fun lists:foldl/3
           end,
    Grouped = Fold(
                fun({V, _}=This, [[{V, _}|_]=Group|Tail]) ->
                        [[This|Group]|Tail];
                   (This, Acc) ->
                        [[This]|Acc]
                end,
                [],
                Sorted),
    {1, [[Id || {_, Id} <- Group] || Group <- Grouped]};
do_sort({Lvl, RscGrps}, Opt, Prop, Context) ->
    Sorted = [do_sort({Lvl - 1, Group}, Opt, Prop, Context) || Group <- RscGrps],
    {Lvl + 1, [Group || {_, Group} <- Sorted]}.

get_prop(Id, Prop, Context) ->
    case m_rsc:p(Id, Prop, Context) of
        {trans, _} = T -> z_trans:lookup(T, Context);
        Value -> Value
    end.

split_opt([CheckOpt|Tail]=Args, Default) ->
    case is_opt(CheckOpt) of
        false ->
            {Default, Args};
        Opt ->
            {Opt, Tail}
    end.

is_opt(Opt) ->
    %% work-around for `lists:member` not being legal to use in if guards... bah !
    %% and I don't want to use nested case statments, so this was the most concise
    %% I could come up with... :p
    proplists:get_value(
      true, 
      [{lists:member(Opt, ascending_keywords()), ascending},
       {lists:member(Opt, descending_keywords()), descending}],
      false).

ascending_keywords() ->
    [ascending, asc, '+', "+", "ascending", "asc"].

descending_keywords() ->
    [descending, desc, '-', "-", "descending", "desc"].
