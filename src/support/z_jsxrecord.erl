%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2020 Maas-Maarten Zeeman
%% @doc Load often used zotonic record definitions to ease json encoding and decoding

%% Copyright 2020 Maas-Maarten Zeeman
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

-compile(nowarn_unused_record).

%% Note: This is a feature backported from Zotonic 1.0, and not all record 
%% definitions are loaded by default.

%% @doc Extra trans record definition to ease JSON mapping of translatable strings
-record(trans, {tr = [] :: list( {atom(), binary()} )}).
    
init() ->
    erlang:spawn( fun() -> jsxrecord:load_records([ ?MODULE ]) end),
    ok.
