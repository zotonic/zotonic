%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% @doc Show a file size, given the size in bytes

%% Copyright 2011 Marc Worrell
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

-module(filter_filesizeformat).
-export([filesizeformat/2]).

-define(KB, 1024).
-define(MB, 1048576).

filesizeformat(N, _Context) when is_integer(N), N < 2*?KB ->
    [integer_to_list(N), " bytes"];
filesizeformat(N, _Context) when is_integer(N), N < ?MB ->
    N1 = trunc(N div ?KB),
    N2 = round(((N / ?KB) - N1)*10),
    case N2 of
        0 -> [integer_to_list(N1), " KB"];
        _ -> [integer_to_list(N1),$.,integer_to_list(N2), " KB"]
    end;
filesizeformat(N, _Context) when is_integer(N) ->
    N1 = trunc(N div ?MB),
    N2 = round(((N / ?MB) - N1)*10),
    case N2 of
        0 -> [integer_to_list(N1), " MB"];
        _ -> [integer_to_list(N1),$.,integer_to_list(N2), " MB"]
    end;
filesizeformat(_, _Context) ->
    undefined.
    