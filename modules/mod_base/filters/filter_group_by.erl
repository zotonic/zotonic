%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'group_by' filter, groups a list of proplists on a property

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

-module(filter_group_by).
-export([group_by/3]).


group_by(In, undefined, _Context) -> 
    In;
group_by(undefined, _, _Context) -> 
    undefined;
group_by(In, Prop, Context) ->
    z_utils:group_by(erlydtl_runtime:to_list(In, Context), z_convert:to_atom(Prop), Context).

