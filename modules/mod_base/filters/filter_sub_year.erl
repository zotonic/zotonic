%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'sub_year' filter, subtract one or more years from a date

%% Copyright 2010 Marc Worrell
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

-module(filter_sub_year).
-export([sub_year/2, sub_year/3]).

sub_year(undefined, _Context) ->
	undefined;
sub_year({{Y,M,D},Time}, _Context) ->
	{{Y-1,M,D},Time}.

sub_year(undefined, _N, _Context) ->
	undefined;
sub_year({{Y,M,D},Time}, N, _Context) ->
	{{Y-N,M,D},Time}.
