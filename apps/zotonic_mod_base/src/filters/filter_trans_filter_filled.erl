%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015-2022 Marc Worrell
%% @doc Filter a #trans{} record, only pass the filled fields.
%% @end

%% Copyright 2015-2022 Marc Worrell
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

-module(filter_trans_filter_filled).
-moduledoc("
Filters all empty translations from a property.

This is used if it is important to show a text, but not all translations are filled in.

The filter takes as input a resource or other variable and as argument the property to be shown.

Example usage:


```none
{{ id|trans_filter_filled:`body` }}
```

If the resource `id` has the body property:


```erlang
{trans, [{en, <<>>}, {nl,<<\"Hallo\">>}]}
```

Then this will show `Hallo`, even if the language is set to `en`.
").

-export([
    trans_filter_filled/2,
    trans_filter_filled/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").

trans_filter_filled(Var, _Context) ->
    filled_in(Var).

trans_filter_filled(Var, Key, Context) ->
    filled_in(z_template_compiler_runtime:find_value(Key, Var, #{}, Context)).

filled_in(#trans{ tr = Tr}) ->
    Tr1 = lists:filter(fun({_,T}) -> is_filled_in(T) end, Tr),
    {trans, Tr1};
filled_in(A) ->
    A.

is_filled_in(<<>>) ->
    false;
is_filled_in(<<C, T/binary>>) when C =:= 32; C=:= 9 ->
    is_filled_in(T);
is_filled_in(<<"<p></p>">>) ->
    false;
is_filled_in(_) ->
    true.
