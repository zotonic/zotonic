%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2010 Maas-Maarten Zeeman
%% @doc 'match' match a value. returns true if the value matches the regular expression.

%% Copyright 2011 Maas-Maarten Zeeman
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

-module(filter_match).
-export([match/3]).

-author('mmzeeman@xs4all.nl').

match(undefined, _Re, _Context) ->
    false;
match(_, undefined, _Context) ->
    false;
match(S, Re, Context) ->
    S1 = z_convert:to_binary(S, Context),
    Re1 = z_convert:to_binary(Re, Context),
    case re:run(S1, Re1) of
	match ->
	    true;
	nomatch ->
	    false;
	{match, _} ->
	    true
    end.

