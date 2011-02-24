%% @author Marc Worrell <marc@worrell.nl>
%% Date: 2010-02-12
%% @copyright 2010 Marc Worrell
%% @doc Encode data to quoted printable strings.

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

-module(z_quoted_printable).

-author("Marc Worrell <marc@worrell.nl>").

-export([
    encode/1
]).


%% @doc Encode a string as quoted printable.
%% @spec encode(iolist()) -> binary()
encode(L) when is_list(L) ->
    encode(iolist_to_binary(L));
encode(B) when is_binary(B) ->
    encode(B, 0, <<>>).
    
    encode(<<>>, _, Acc) ->
        Acc;
    encode(B, Len, Acc) when Len >= 72 ->
        encode(B, 0, <<Acc/binary, $=, 13, 10>>);
    encode(<<C,Rest/binary>>, Len, Acc) when C < 32 orelse C >= 127 orelse C =:= $= orelse C =:= $. ->
        H1 = to_hex(C div 16),
        H2 = to_hex(C rem 16),
        encode(Rest, Len+3, <<Acc/binary, $=, H1, H2>>);
    encode(<<C,Rest/binary>>, Len, Acc) ->
        encode(Rest, Len+1, <<Acc/binary, C>>).


to_hex(C) when C < 10 -> 
    C + $0;
to_hex(C) -> 
    C - 10 + $A.
