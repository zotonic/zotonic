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

%% Modified by François Cardinaux in order to convert UTF8 strings correctly
%% Usage: 
%%    * 8-bit character encoding (ASCII, ISO8859-x, etc.): 
%%          {{ value|to_json }}
%%    * UTF-8 character encoding: 
%%          {{ value|to_json:"utf8" }}
%%    * Unicode character encoding, as defined in the mochijson module: 
%%          {{ value|to_json:"unicode" }}

-module(filter_to_json).
-export([to_json/2, to_json/3]).

%% @doc Convert an Erlang list or tuple to JSON
%% @spec to_json(ErlangTerm, Context) -> Json
%% Where: 
%%    * ErlangTerm = list() | tuple()
%%    * Context = Zotonic context record
%%    * Json = JSON content
to_json(Value, _Context) ->
    mochijson:encode(z_convert:to_json(Value)).
    
%% @doc Convert an Erlang list or tuple to JSON
%% @spec to_json(ErlangTerm, Encoding, Context) -> Json
%% Where: 
%%    * ErlangTerm, Context and Json: like in to_json/2
%%    * Encoding = 
%%        * "utf8": ErlangTerm contains UTF8 strings
%%        * "unicode": ErlangTerm contains unicode strings, as defined in the mochijson module
to_json(Value, "utf8", _Context) ->
    Encoder = mochijson:encoder([{input_encoding, utf8}]),
    Encoder(z_convert:to_json(Value));
    
to_json(Value, "unicode", _Context) ->
    Encoder = mochijson:encoder([{input_encoding, unicode}]),
    Encoder(z_convert:to_json(Value));

to_json(Value, _Encoding, Context) ->
    to_json(Value, Context).
    

