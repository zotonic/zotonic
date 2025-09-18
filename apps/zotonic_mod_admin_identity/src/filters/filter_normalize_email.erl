%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2022 Marc Worrell
%% @doc Normalize an email address, as used for the keys in the identity table.
%% @end

%% Copyright 2022 Marc Worrell
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

-module(filter_normalize_email).
-moduledoc("
Normalize an email address, used in the identity management.

The email address is lowercased and trimmed.

For example:


```django
{{ \"ME@Example.Com \"|normalize_email }}
```

Evaluates to the value `me@example.com`.
").

-export([ normalize_email/2 ]).

normalize_email(undefined, _Context) ->
    undefined;
normalize_email(Email, _Context) when is_binary(Email) ->
    m_identity:normalize_key(email, Email);
normalize_email(Email, Context) ->
    normalize_email(z_convert:to_binary(Email), Context).
