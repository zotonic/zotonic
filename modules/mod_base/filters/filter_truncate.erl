%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'truncate' filter, truncate a string on a certain length, taking word boundaries into account.

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

-module(filter_truncate).
-export([truncate/2, truncate/3, truncate/4]).

truncate(In, Context) ->
    truncate(In, 20, Context).

truncate(In, N, Context) ->
    truncate(In, N, <<226,128,166>>, Context).

truncate(undefined, _N, _Append, _Context) ->
    undefined;
truncate(S, N, Append, Context) when not is_integer(N) ->
    truncate(S, z_convert:to_integer(N), Append, Context);
truncate({tr, _} = Tr, N, Append, Context) ->
    truncate(z_trans:lookup_fallback(Tr, Context), N, Append, Context);
truncate(In, N, Append, _Context) when is_binary(In) ->
    z_string:truncate(In, N, z_convert:to_binary(Append));
truncate(In, N, Append, _Context) when is_list(In) ->
    z_string:truncate(iolist_to_binary(In), N, z_convert:to_binary(Append));
truncate(In, N, Append, Context) ->
    case erlydtl_runtime:to_value(In, Context) of
        L when is_list(L) ->
            truncate(L, N, Append, Context);
        B when is_binary(B) ->
            truncate(B, N, Append, Context);
        _ ->
            undefined
    end.

