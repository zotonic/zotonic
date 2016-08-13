%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'vsplit_in' filter, split a list in a number of columns

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

-module(filter_vsplit_in).
-export([vsplit_in/3]).


vsplit_in(undefined, _N, _Context) ->
    undefined;
vsplit_in(In, N, Context) ->
    z_utils:vsplit_in(z_template_compiler_runtime:to_list(In, Context), N).
