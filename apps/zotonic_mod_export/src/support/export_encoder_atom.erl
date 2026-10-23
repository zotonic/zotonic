%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017-2026 Marc Worrell <marc@worrell.nl>
%% @doc Format exports for atom format
%% @end

%% Copyright 2017-2026 Marc Worrell
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

-module(export_encoder_atom).

-export([extension/0, mime/0, init/2, header/3, row/3, footer/3]).

extension() ->
    [<<"atom">>].

mime() ->
    [{<<"application">>, <<"atom+xml">>, []}].

init(Options, Context) ->
    export_encoder_xml:init([{feed_format, atom} | Options], Context).

header(Header, State, Context) ->
    export_encoder_xml:header(Header, State, Context).

row(Row, State, Context) ->
    export_encoder_xml:row(Row, State, Context).

footer(Data, State, Context) ->
    export_encoder_xml:footer(Data, State, Context).
