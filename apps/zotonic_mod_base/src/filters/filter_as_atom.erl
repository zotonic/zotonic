%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc Convert a value to an atom

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

-module(filter_as_atom).
-moduledoc("
Convert a value to an Erlang atom.

Atoms are represented in the template language as strings between backticks, `` `like this` ``. Some template function
require their arguments to be atoms, and this filter helps in converting any value to such an atom:


```django
{{ \"foobar\"|as_atom }}
```

evaluates to the atom `foobar`.
").
-export([as_atom/2]).

as_atom(V, _Context) ->
	z_convert:to_atom(V).
