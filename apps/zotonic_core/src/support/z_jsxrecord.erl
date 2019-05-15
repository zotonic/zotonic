%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017 Marc Worrell
%% @doc Define records for JSON encoding

%% Copyright 2017 Marc Worrell
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

-module(z_jsxrecord).

-export([
    init/0
]).

-include("zotonic.hrl").

-compile(nowarn_unused_record).

%% Used in EXIF
-record(ratio, {
        numerator :: integer(),
        denominator :: integer()
    }).

%% @doc Let jsxrecord load the Zotonic record definitions from this module
init() ->
    erlang:spawn( fun() -> jsxrecord:load_records([ ?MODULE ]) end),
    application:set_env(mqtt_sessions, json_encoder, jsxrecord).
