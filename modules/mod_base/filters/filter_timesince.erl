%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'timesince' filter, show a textual representation how far a date is from now.

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

-module(filter_timesince).
-export([timesince/2, timesince/3, timesince/4]).


timesince(undefined, _Context) ->
    undefined;
timesince(Date, Context) ->
	z_datetime:timesince(Date, Context).
timesince(undefined, _Base, _Context) ->
    undefined;
timesince(Date, Base, Context) ->
	z_datetime:timesince(Date, Base, Context).
timesince(Date, Base, When, Context) ->
    z_datetime:timesince(Date, Base, When, Context).

