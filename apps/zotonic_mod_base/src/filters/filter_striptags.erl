%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2024 Marc Worrell
%% @doc 'striptags' filter, remove (x)html tags
%% @end

%% Copyright 2010-2024 Marc Worrell
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

-module(filter_striptags).

-export([striptags/2, striptags/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Strip the HTML tags from a string.
-spec striptags(In, Context) -> binary() | undefined when
    In :: undefined | iodata() | #trans{} | atom() | integer() | float(),
    Context :: z:context().
striptags(undefined, _Context) ->
    undefined;
striptags(null, _Context) ->
    undefined;
striptags(In, _Context) when is_integer(In) ->
    In;
striptags(In, _Context) when is_float(In) ->
    In;
striptags(In, Context) when is_atom(In) ->
    striptags(atom_to_binary(In, utf8), Context);
striptags(#trans{} = Trans, Context) ->
    z_html:strip(z_trans:lookup_fallback(Trans, Context));
striptags(In, _Context) when is_binary(In); is_list(In) ->
    z_html:strip(In);
striptags(_In, _Context) ->
    undefined.

%% @doc Strip the HTML tags from a string, return at most MaxLen characters.
-spec striptags(In, MaxLen, Context) -> binary() | undefined when
    In :: undefined | iodata() | #trans{} | atom() | integer() | float(),
    MaxLen :: integer() | binary(),
    Context :: z:context().
striptags(undefined, _MaxLen, _Context) ->
    undefined;
striptags(null, _MaxLen, _Context) ->
    undefined;
striptags(In, _MaxLen, _Context) when is_integer(In) ->
    In;
striptags(In, _MaxLen, _Context) when is_float(In) ->
    In;
striptags(In, MaxLen, Context) when is_atom(In) ->
    striptags(atom_to_binary(In, utf8), MaxLen, Context);
striptags(#trans{} = Trans, MaxLen, Context) ->
    z_html:strip(z_trans:lookup_fallback(Trans, Context), z_convert:to_integer(MaxLen));
striptags(In, MaxLen, _Context) when is_binary(In); is_list(In) ->
    z_html:strip(In, z_convert:to_integer(MaxLen));
striptags(_In, _MaxLen, _Context) ->
    undefined.
