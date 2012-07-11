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

-module(filter_survey_prepare_matching).

-export([
    survey_prepare_matching/2
]).

survey_prepare_matching(Blk, Context) ->
    Matching = z_trans:lookup_fallback(
                    proplists:get_value(matching, Blk, <<>>), 
                    Context),
    Pairs = [ split_option(Line) || Line <- split_lines(Matching) ],
    {Qs,As} = lists:unzip(Pairs),
    Qs1 = split_markers(Qs),
    As1 = split_markers(As),
    [
        {items, Qs1},
        {options, z_utils:randomize(As1)}
    ].

split_lines(Text) ->
    Options = string:tokens(z_string:trim(z_convert:to_list(Text)), "\n"),
    [ z_string:trim(Option) || Option <- Options ].

split_option(Option) ->
    {Q,M} = lists:splitwith(fun(C) -> C /= $= end, Option),
    {z_string:trim(Q), z_string:trim(drop_eq(M))}.

    drop_eq([$=|Rest]) -> Rest;
    drop_eq(X) -> X.

split_markers(Qs) ->
    [ split_marker(Opt) || Opt <- Qs, Opt /= [] ].

split_marker(X) ->
    case lists:splitwith(fun(C) -> C /= $# end, X) of
        {Opt, []} -> {Opt, Opt};
        {Opt, [$#|Rest]} -> {Opt, Rest}
    end.
