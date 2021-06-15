%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'join' filter, join the values of a list, with an optional separator.

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

-module(filter_join).
-export([join/2, join/3, join/4]).


-include_lib("zotonic_core/include/zotonic.hrl").

join(Input, Context) ->
    join(Input, <<>>, Context).

join(Input, <<>>, Context) when is_list(Input) ->
    List1 = [ z_convert:to_binary(X, Context) || X <- Input ],
    iolist_to_binary(List1);
join(Input, Separator, Context) when is_list(Input) ->
    List1 = [ z_convert:to_binary(X, Context) || X <- Input ],
    iolist_to_binary(
        lists:join(
            z_convert:to_binary(Separator, Context),
            List1));
join(Input, _, _Context) ->
    Input.

join(Input, Separator, _LastSeparator, Context) when length(Input) =< 1 ->
    join(Input, Separator, Context);
join(Input, Separator, <<32, _Rest/binary>> = LastSeparator, Context) ->
    join([join(lists:droplast(Input), Separator, Context),
          lists:last(Input)],
         LastSeparator, Context);
join(Input, Separator, LastSeparator, Context) ->
    %% Translated strings will have leading spaces tripped, add them here.
    join(Input, Separator, <<32, (z_convert:to_binary(LastSeparator))/binary, 32>>, Context).

