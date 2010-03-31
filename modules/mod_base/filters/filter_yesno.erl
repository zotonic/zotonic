%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'yesno' filter, textual representations of a boolean value

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

-module(filter_yesno).
-export([yesno/2, yesno/3]).


yesno(B, _Context) ->
    case erlydtl_runtime:is_false(B) of
        true -> "no";
        false -> "yes"
    end.
yesno(undefined, Values, _Context) ->
    case string:tokens(z_convert:to_list(Values), ",") of
        [_Yes, _No, Maybe] -> Maybe;
        [_Yes, No] -> No
    end;
yesno(B, Values, _Context) ->
    case erlydtl_runtime:is_false(B) of
        true ->
            [_Yes,No|_Rest] = string:tokens(z_convert:to_list(Values), ","),
            No;
        false -> 
            [Yes|_Rest] = string:tokens(z_convert:to_list(Values), ","),
            Yes
    end.

