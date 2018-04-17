%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2018  Marc Worrell
%% @doc Ensure a mixed rendering becomes a valid iolist.

%% Copyright 2018 Marc Worrell
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

-module(z_output_html).

-export([
    output/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Replace non-iolist parts in the output tree.
-spec output( term(), z:context() ) -> {iolist(), z:context()}.
output(MixedHtml, Context) ->
    {MixedHtml1, Context1} = z_notifier:foldl(
        #output_html{ html = MixedHtml },
        {MixedHtml, Context},
        Context),
    output1(MixedHtml1, Context1, []).



%% @doc Recursively walk through the output, replacing all non iolist data.
output1(undefined, Context, Acc) ->
    {lists:reverse(Acc), Context};
output1(<<>>, Context, Acc) ->
    {lists:reverse(Acc), Context};
output1([], Context, Acc) ->
    {lists:reverse(Acc), Context};
output1(B, Context, Acc) when is_binary(B) ->
    {[ lists:reverse(Acc), B ], Context};
output1([List|Rest], Context, Acc) when is_list(List) ->
    {Rendered, Context1} = output1(List, Context, []),
    output1(Rest, Context1, [ Rendered | Acc ]);
output1([ undefined | Rest], Context, Acc) ->
    output1(Rest, Context, Acc);
output1([ C |Rest ], Context, Acc) when is_atom(C) ->
    output1(Rest, Context, [ atom_to_binary(C, utf8) | Acc ]);
output1([ {trans, _} = Trans | Rest ], Context, Acc) ->
    output1(Rest, Context, [ z_trans:lookup_fallback(Trans, Context) | Acc ]);
output1([ {{_,_,_},{_,_,_}} = D | Rest ], Context, Acc) ->
    output1([filter_date:date(D, "Y-m-d H:i:s", Context)|Rest], Context, Acc);
output1([ T | Rest ], Context, Acc) when is_tuple(T) ->
    output1([iolist_to_binary(io_lib:format("~p", [T]))|Rest], Context, Acc);
output1([ C | Rest ], Context, Acc) when is_integer(C), C >= 0, C =< 255->
    output1(Rest, Context, [ C | Acc ]);
output1([ C | Rest ], Context, Acc) when is_integer(C), C >= 0 ->
    output1(Rest, Context, [ <<C/utf8>> | Acc ]);
output1([ C | Rest ], Context, Acc) ->
    output1(Rest, Context, [ z_convert:to_binary(C) | Acc ]).

