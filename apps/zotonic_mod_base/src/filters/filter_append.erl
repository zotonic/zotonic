%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'append' filter, concatenate two values as lists.

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

-module(filter_append).
-export([append/3]).


append(Input, undefined, _Context) ->
    Input;
append(undefined, Append, _Context) ->
    Append;
append({trans, _} = Tr, Append, Context) ->
    append(z_trans:lookup_fallback(Tr, Context), Append, Context);
append(Input, {trans, _} = Tr, Context) ->
    append(Input, z_trans:lookup_fallback(Tr, Context), Context);
append(Input, Append, _Context) when is_binary(Input), is_binary(Append) ->
    <<Input/binary, Append/binary>>;
append(Input, Append, Context) ->
    z_template_compiler_runtime:to_list(Input, Context)
    ++ z_template_compiler_runtime:to_list(Append, Context).


