%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Marc Worrell
%% @doc 'as_name' filter, translate a string to a 'name'

%% Copyright 2019 Marc Worrell
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

-module(filter_to_name).
-export([to_name/2]).


to_name(undefined, _Context) ->
    undefined;
to_name({trans, _} = Tr, Context) ->
    to_name(z_trans:lookup_fallback(Tr, Context), Context);
to_name(Input, _Context) when is_list(Input); is_binary(Input) ->
    z_string:to_name(Input);
to_name(Input, Context) ->
    to_name( z_convert:to_binary(Input), Context ).
