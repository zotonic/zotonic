%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014 Marc Worrell
%% @doc 'truncate_html' filter, truncate a html string on a certain length, assuming the html is sanitized.

%% Copyright 2014 Marc Worrell
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

-module(filter_truncate_html).
-export([truncate_html/2, truncate_html/3, truncate_html/4]).

truncate_html(In, Context) ->
    truncate_html(In, 20, Context).

truncate_html(In, N, Context) ->
    truncate_html(In, N, <<226,128,166>>, Context).

truncate_html(undefined, _N, _Append, _Context) ->
    undefined;
truncate_html(S, N, Append, Context) when not is_integer(N) ->
    truncate_html(S, z_convert:to_integer(N), Append, Context);
truncate_html({tr, _} = Tr, N, Append, Context) ->
    truncate_html(z_trans:lookup_fallback(Tr, Context), N, Append, Context);
truncate_html(In, N, Append, _Context) when is_binary(In) ->
    z_html:truncate(In, N, z_convert:to_binary(Append));
truncate_html(In, N, Append, _Context) when is_list(In) ->
    z_html:truncate(iolist_to_binary(In), N, z_convert:to_binary(Append));
truncate_html(In, N, Append, Context) ->
    case erlydtl_runtime:to_value(In, Context) of
        L when is_list(L) ->
            truncate_html(L, N, Append, Context);
        B when is_binary(B) ->
            truncate_html(B, N, Append, Context);
        _ ->
            undefined
    end.

