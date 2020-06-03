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
                    maps:get(<<"narrative">>, Blk, <<>>),
                    Context),
    {Parts, _Inputs} = parse(Narrative),
    [
        {parts, Parts}
    ].


parse(Text) ->
    parse(Text, [], []).

parse(<<>>, Acc, InputAcc) ->
    {lists:reverse(Acc), InputAcc};
parse(<<$[, T/binary>>, Acc, InputAcc) ->
    [ Input1, T1 ] = case binary:split(T, <<"]">>) of
        [ _, _ ] = Split -> Split;
        [ A ] -> [ A, <<>> ]
    end,
    IsSelect = is_select(Input1),
    Elt = case IsSelect of
        true ->
            {Name, Options} = split_select(Input1),
            {select, z_convert:to_binary(Name), filter_survey_prepare_matching:split_markers(Options)};
        false ->
            Name = z_string:trim(Input1),
            Length = length(Input1),
            {input, Name, Length}
    end,
    Acc1 = [Elt|Acc],
    InputAcc1 = [{IsSelect, Name}|InputAcc],
    parse(T1, Acc1, InputAcc1);
parse(T, Acc, InputAcc) ->
    [ Text, Rest ] = case binary:split(T, <<"[">>) of
        [ A, R ] -> [ A, <<"[", R/binary>> ];
        [ A ] -> [ A, <<>> ]
    end,
    Escaped = z_html:escape(Text),
    WithBr = binary:replace(Escaped, <<10>>, <<"<br>">>, [ global ]),
    Acc1 = [{html, [], iolist_to_binary(WithBr)}|Acc],
    parse(Rest, Acc1, InputAcc).


is_select(<<>>) -> false;
is_select(<<$=, _/binary>>) -> true;
is_select(<<$|, _/binary>>) -> true;
is_select(<<_, T/binary>>) -> is_select(T).

split_select(I) ->
    {Name, Options} = split_name(I),
    Options1 = [ z_string:trim(Opt) || Opt <- binary:split(Options, <<"|">>, [ global, trim_all ]) ],
    {z_string:trim(Name), Options1}.

split_name(Name) ->
    case binary:split(Name, <<"=">>, [ trim ]) of
        [ Name, Options ] -> {Name, Options};
        [ Name ] -> {Name, <<>>}
    end.
