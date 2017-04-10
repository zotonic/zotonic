%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%% @copyright 2010 Dmitrii Dimandt, Marc Worrell
%% @doc 'unjoin' filter, "reverse" of join, split a string into a list.

%% Copyright 2010 Dmitrii Dimandt, Marc Worrell
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

-module(filter_split).
-export([split/3]).


split(undefined, _Sep, _Context) ->
	undefined;
split(<<>>, _Sep, _Context) ->
	[];
split([], _Sep, _Context) ->
	[];
split({trans, _} = Tr, Sep, Context) ->
    split(z_trans:lookup_fallback(Tr, Context), Sep, Context);
split(String, {trans, _} = Tr, Context) ->
    split(String, z_trans:lookup_fallback(Tr, Context), Context);
split(String, Sep, _Context) when is_binary(String), is_binary(Sep) ->
	binary:split(String, Sep, [global]);
split(String, Sep, _Context) ->
    z_string:split(z_convert:to_list(String), z_convert:to_list(Sep)).

