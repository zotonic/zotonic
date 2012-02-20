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
%%    * If the input value contains strings of UTF-8-encoded characters: 
%%          {{ value|to_json }}
%%    * If the input value contains strings of ASCII or ISO 8859-1 (= Latin-1) characters: 
%%          {{ value|to_json:"latin-1" }}
%%    * If the input value contains strings of unicode characters, as defined in the mochijson module: 
%%          {{ value|to_json:"unicode" }}
%% Note that these variants only concern the strings in the input term. 
%% The output will always contain utf-8-encoded strings.  

-module(filter_to_json).
-export([to_json/2, to_json/3]).

%% @doc Convert an Erlang list or tuple to JSON.
%% This function assumes that the input contains only strings that are composed of utf-8 characters. 
%% For other encodings, use to_json/3 instead.
%% @spec to_json(ErlangTerm, Context) -> Json
%% Where: 
%%    * ErlangTerm = list() | tuple()
%%    * Context = Zotonic context record
%%    * Json = JSON content
to_json(Value, _Context) ->
    Encoder = mochijson:encoder([{input_encoding, utf8}]),
    Encoder(z_convert:to_json(Value)).
    
%% @doc Convert an Erlang list or tuple to JSON
%% @spec to_json(ErlangTerm, Encoding, Context) -> Json
%% Where: 
%%    * ErlangTerm, Context and Json: like in to_json/2
%%    * Encoding: the character encoding of the strings in the input term (the output always contains utf-8 character strings)
%%      The possible values are: 
%%        * "latin-1": ErlangTerm contains strings of ISO 8859-1 characters (this character set encompasses ASCII characters)
%%        * "unicode": ErlangTerm contains unicode strings, as defined in the mochijson module
%%      For utf-8 input strings, use to_json/2 instead.
to_json(Value, "latin-1", _Context) ->
    mochijson:encode(z_convert:to_json(Value));
    
to_json(Value, "unicode", _Context) ->
    Encoder = mochijson:encoder([{input_encoding, unicode}]),
    Encoder(z_convert:to_json(Value));

to_json(Value, _Encoding, Context) ->
    to_json(Value, Context).

