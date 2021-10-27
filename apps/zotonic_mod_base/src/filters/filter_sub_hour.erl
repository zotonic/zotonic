%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2021 Maas-Maarten Zeeman
%% @doc 'sub_hour' filter, subtract hours from a date

%% Copyright 2021 Maas-Maarten Zeeman 
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

-module(filter_sub_hour).
-export([sub_hour/2, sub_hour/3]).

sub_hour(undefined, _Context) ->
	undefined;
sub_hour(Date, Context) ->
	filter_add_hour:add_hour(Date, -1, Context).
sub_hour(undefined, _N, _Context) ->
	undefined;
sub_hour(Date, N, Context) ->
	filter_add_hour:add_hour(Date, 0-N, Context).

