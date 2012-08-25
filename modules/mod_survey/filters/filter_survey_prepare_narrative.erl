%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell

%% Copyright 2012 Marc Worrell
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

-module(filter_survey_prepare_narrative).

-export([
    survey_prepare_narrative/2,
    parse/1
]).

survey_prepare_narrative(Blk, Context) ->
    Narrative = z_trans:lookup_fallback(
                    proplists:get_value(narrative, Blk, <<>>), 
                    Context),
    {Parts, _Inputs} = parse(z_convert:to_list(Narrative)),
    [
        {parts, Parts}
    ].


parse(Text) ->
    parse(Text, [], []).

parse([], Acc, InputAcc) ->
    {lists:reverse(Acc), InputAcc};
parse([$[|T], Acc, InputAcc) ->
    {Input1,T1} = lists:splitwith(fun(C) -> C /= $] end, T),
    IsSelect = is_select(Input1),
    Elt = case IsSelect of
        true -> 
            {Name, Options} = split_select(Input1),
            {select, z_convert:to_binary(Name), filter_survey_prepare_matching:split_markers(Options)};
        false ->
            Name = z_string:trim(Input1),
            Length = length(Input1),
            {input, z_convert:to_binary(Name), Length}
    end,
    Acc1 = [Elt|Acc],
    InputAcc1 = [{IsSelect, Name}|InputAcc],
    case T1 of
        [] -> parse([], Acc1, InputAcc1);
        [$]|T2] -> parse(T2, Acc1, InputAcc1)
    end;
parse(T, Acc, InputAcc) ->
    {Text,Rest} = lists:splitwith(fun(C) -> C /= $[ end, T),
    Text1 = lists:map(fun(10) -> "<br/>";
                         (C) -> C
                      end,
                      z_convert:to_list(z_html:escape(Text))),
    Acc1 = [{html, [], iolist_to_binary(Text1)}|Acc],
    parse(Rest, Acc1, InputAcc).


is_select([]) -> false;
is_select([$=|_]) -> true;
is_select([$||_]) -> true;
is_select([_|T]) -> is_select(T).


split_select(I) ->
    {Name, Options} = split_name(I, []),
    Options1 = [ z_string:trim(Opt) || Opt <- string:tokens(Options, "|") ],
    {z_string:trim(Name), Options1}.

split_name([], Acc) ->
    {"", lists:reverse(Acc)};
split_name([$=|Rest], Acc) ->
    {lists:reverse(Acc), Rest};
split_name([H|T], Acc) ->
    split_name(T, [H|Acc]).
