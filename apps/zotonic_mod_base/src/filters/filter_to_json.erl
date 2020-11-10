%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse
%% @doc Convert a value to json

%% Copyright 2011 Arjan Scherpenisse
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

-module(filter_to_json).
-export([to_json/2]).

%% @doc Convert an Erlang list or tuple to JSON
%% This function assumes that all strings of the input term are made of utf-8-encoded characters.
-spec to_json( term(), z:context() ) -> iodata().
to_json(Value, _Context) ->
    z_json:encode(Value).
