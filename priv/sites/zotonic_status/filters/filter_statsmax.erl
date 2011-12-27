%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% Date: 2011-12-27

%% @doc jQuery compatible key for statistics

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

-module(filter_statsmax).
-author("Marc Worrell <marc@worrell.nl>").

-include_lib("include/zotonic.hrl").

-export([statsmax/2]).

statsmax(Stat, _Context) ->
    erlang:max(
        max_array(proplists:get_value(second, Stat, [1])),
        erlang:max(proplists:get_value(perc95, Stat, 1), 
                   proplists:get_value(avg, Stat, 1))
    ) * 11 div 10.

max_array(L) ->
    lists:max([X || {X,_} <- L]).
