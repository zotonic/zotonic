%% @author Dirk Geurs <dirk@driebit.nl>
%% @copyright 2015 Marc Worrell
%% @doc Force the input to a value

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

-module(filter_make_value).
-export([make_value/2]).

make_value(In, Context) ->
    erlydtl_runtime:to_value(In, Context).
