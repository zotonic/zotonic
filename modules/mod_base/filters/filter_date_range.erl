%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2014 Marc Worrell
%% @doc 'date_range' filter, display two dates

%% Copyright 2010-2014 Marc Worrell
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

-module(filter_date_range).
-export([
	date_range/3,
	date_range/4
]).

date_range(Dates, Format, true, Context) ->
    date_range(Dates, Format, z_context:set_tz(<<"UTC">>,Context));
date_range(Dates, Format, false, Context) ->
    date_range(Dates, Format, Context);
date_range(Dates, Format, undefined, Context) ->
    date_range(Dates, Format, Context);
date_range(Dates, Format, [], Context) ->
    date_range(Dates, Format, Context);
date_range(Dates, Format, <<>>, Context) ->
    date_range(Dates, Format, Context);
date_range(Dates, Format, Tz, Context) ->
    date_range(Dates, Format, z_context:set_tz(Tz,Context)).

date_range([A, B], [WithDate, Sep, EqDate], Context) ->
	ALocal = z_datetime:to_local(A, Context),
	BLocal = z_datetime:to_local(B, Context),
    case filter_eq_day:eq_day(ALocal, BLocal, Context) of
        true -> [ filter_date:date(A, WithDate, Context), Sep, filter_date:date(B, EqDate, Context) ];
        false -> [ filter_date:date(A, WithDate, Context), Sep, filter_date:date(B, WithDate, Context) ]
    end.

