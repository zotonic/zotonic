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

%% Modified by François Cardinaux in order to convert UTF8 strings correctly.
%% Usage: 
%%    * If the input value contains strings of UTF-8-encoded characters: 
%%          {{ value|to_json:"utf-8" }}
%%    * If the input value contains strings of ISO 8859-1 (= Latin-1) characters: 
%%          {{ value|to_json:"latin-1" }}
%% Note that these variants only concern the strings in the input term. 
%% The output will always contain utf-8-encoded strings.  

-module(filter_to_json).
-export([to_json/3]).

%% @doc Convert an Erlang list or tuple to JSON
%% This function assumes that the all strings of the input term have the same 
%% character encoding. This encoding may be UTF-8 or ISO 8859-1 (also called Latin-1). 
%% @spec to_json(ErlangTerm, Encoding, Context) -> Json
%% Where: 
%%    * ErlangTerm = list() | tuple()
%%    * Encoding = the character encoding of the strings in the input term. 
%%      Following values are accepted: 
%%        * "utf-8": ErlangTerm contains strings of UTF-8-encoded characters
%%        * "latin-1": ErlangTerm contains strings of ISO 8859-1 characters
%%      Note that this parameter concerns the strings in the input term only. 
%%      The JSON output will always contain utf-8-encoded strings.  
%%    * Context = Zotonic context record
%%    * Json = the JSON content
to_json(Value, "latin-1", _Context) ->
    mochijson:encode(z_convert:to_json(Value));
    
to_json(Value, "utf-8", _Context) ->
    Encoder = mochijson:encoder([{input_encoding, utf8}]),
    Encoder(z_convert:to_json(Value)).

