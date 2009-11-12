%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-11-04
%%
%% @doc Utility functions for datetime handling and representation.

%% Copyright 2009 Marc Worrell
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

-module(z_datetime).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    timesince/2,
	timesince/3,
	
	last_day_of_month/2,
	days_in_year/1,
	
	prev_year/1,
	prev_month/1,
	prev_day/1,
	prev_hour/1,
	prev_minute/1,
	prev_second/1,
	
	next_year/1,
	next_month/1,
	next_day/1,
	next_hour/1,
	next_minute/1,
	next_second/1,
	
	diff/2
]).


-include_lib("include/zotonic.hrl").


%% @doc Show a humanized version of a relative datetime.  Like "4 months, 3 days ago".
%% @spec timesince(Date, Context) -> string()
timesince(Date, Context) ->
	timesince(Date, calendar:local_time(), Context).

%% @doc Show a humanized version of a period between two dates.  Like "4 months, 3 days ago".
%% @spec timesince(Date, BaseDate, Context) -> string()
%% @todo Use the language in the context for translations.
timesince(Date, Base, _Context) when Date == Base ->
	"now";
timesince(Date, Base, _Context) when Date > Base ->
	"in " ++ combine(reldate(Base, Date));
timesince(Date, Base, _Context) ->
	combine(reldate(Date, Base)) ++ " ago".	

	combine({A,""}) -> A;
	combine({A,B}) -> A ++ [$,, 32 | B].


%% @doc Return a string describing the relative date difference.
reldate(D1,D2) ->
	case diff(D1,D2) of
		{{0,0,0},{0,0,0}} -> {"now",[]};
		{{0,0,0},{0,0,S}} when S < 10 -> {"moments",[]};
		{{0,0,0},{0,0,S}} -> {plural(S, "second", "seconds"), []};
		{{0,0,0},{0,I,S}} -> {plural(I, "minute", "minutes"), plural(S, "second", "seconds")};
		{{0,0,0},{H,I,_}} -> {plural(H, "hour", "hours"), plural(I, "minute", "minutes")};
		{{0,0,D},{H,_,_}} -> {plural(D, "day", "days"), plural(H, "hour", "hours")};
		{{0,M,D},{_,_,_}} -> {plural(M, "month", "months"), plural(D, "day", "days")};
		{{Y,M,_},{_,_,_}} -> {plural(Y, "year", "years"), plural(M, "month", "months")}
	end.
		

plural(0,_Single,_Plural) ->
	"";
plural(1,Single,_Plural) ->
	"1 " ++ Single;
plural(N,_Single,Plural) ->
	integer_to_list(N) ++ [32 | Plural].


%% @doc Return the date one year earlier.
prev_year({{Y,M,D},T}) ->
	{{Y-1,M,D}, T}.

%% @doc Return the date one month earlier.
prev_month({{Y,1,D},T}) -> {{Y-1,12,D},T};
prev_month({{Y,M,D},T}) -> {{Y,M-1,D}, T}.

%% @doc Return the date one day earlier.
prev_day({{_,_,1},_} = Date) ->
	{{Y1,M1,_},T1} = prev_month(Date),
	{{Y1,M1,last_day_of_month(Y1,M1)}, T1};
prev_day({{Y,M,D},T}) ->
	{{Y,M,D-1}, T}.
	
%% @doc Return the date one hour earlier.
prev_hour({_,{0,_,_}} = Date) ->
	{YMD,{_,I,S}} = prev_day(Date),
	{YMD,{23,I,S}};
prev_hour({YMD,{H,I,S}}) ->
	{YMD, {H-1,I,S}}.

%% @doc Return the date one minute earlier.
prev_minute({_,{_,0,_}} = Date) ->
	{YMD,{H,_,S}} = prev_hour(Date),
	{YMD,{H,59,S}};
prev_minute({YMD,{H,I,S}}) ->
	{YMD, {H,I-1,S}}.

%% @doc Return the date one second earlier.
prev_second({_,{_,_,0}} = Date) ->
	{YMD,{H,I,_}} = prev_minute(Date),
	{YMD,{H,I,59}};
prev_second({YMD,{H,I,S}}) ->
	{YMD, {H,I,S-1}}.

%% @doc Return the date one year later.
next_year({{Y,M,D},T}) ->
	{{Y+1,M,D}, T}.

%% @doc Return the date one month later.
next_month({{Y,12,D},T}) -> {{Y+1,1,D},T};
next_month({{Y,M,D},T}) -> {{Y,M+1,D}, T}.

%% @doc Return the date one day later.
next_day({{Y,M,D},T} = Date) ->
	case last_day_of_month(Y,M) of
		D -> 
			{{Y1,M1,_},T1} = next_month(Date),
			{{Y1,M1,1},T1};
		_ ->
			{{Y,M,D+1},T}
	end.

%% @doc Return the date one hour later.
next_hour({_,{23,_,_}} = Date) ->
	{YMD,{_,I,S}} = next_day(Date),
	{YMD,{0,I,S}};
next_hour({YMD,{H,I,S}}) ->
	{YMD, {H+1,I,S}}.

%% @doc Return the date one minute later.
next_minute({_,{_,59,_}} = Date) ->
	{YMD,{H,_,S}} = next_hour(Date),
	{YMD,{H,0,S}};
next_minute({YMD,{H,I,S}}) ->
	{YMD, {H,I+1,S}}.

%% @doc Return the date one second later.
next_second({_,{_,_,59}} = Date) ->
	{YMD,{H,I,_}} = next_minute(Date),
	{YMD,{H,I,0}};
next_second({YMD,{H,I,S}}) ->
	{YMD, {H,I,S+1}}.

%% @doc Return the last day of the month in a certain year.
last_day_of_month(Y,2) ->
	case calendar:is_leap_year(Y) of
		true -> 29;
   		false -> 28
	end;
last_day_of_month(_,M) 
	when M == 1; M == 3; M == 5; M == 7; M == 8; M == 10; M == 12 ->
	31;
last_day_of_month(_,_) ->
	30.

%% @doc Return the number of days in a certain year.
days_in_year(Y) ->
	case calendar:is_leap_year(Y) of
		true -> 366;
   		false -> 365
	end.

%% @doc Return the absolute difference between two dates.  Does not take daylight saving into account.
diff(Date1, Date2) when Date1 < Date2 ->
	diff(Date2,Date1);
diff({YMD1,{H1,I1,S1}}, {_,{_,_,S2}} = Date2) when S2 > S1 ->
	NextDate2 = next_minute(Date2),
	diff({YMD1,{H1,I1,S1+60}},NextDate2);
diff({YMD1,{H1,I1,S1}}, {_,{_,I2,_}} = Date2) when I2 > I1 ->
	NextDate2 = next_hour(Date2),
	diff({YMD1,{H1,I1+60,S1}},NextDate2);
diff({YMD1,{H1,I1,S1}}, {_,{H2,_,_}} = Date2) when H2 > H1 ->
	NextDate2 = next_day(Date2),
	diff({YMD1,{H1+24,I1,S1}},NextDate2);
diff({{Y1,M1,D1},T1}, {{Y2,M2,D2},_} = Date2) when D2 > D1 ->
	NextDate2 = next_month(Date2),
	diff({{Y1,M1,D1+last_day_of_month(Y2,M2)},T1},NextDate2);
diff({{Y1,M1,D1},T1}, {{_,M2,_},_} = Date2) when M2 > M1 ->
	NextDate2 = next_year(Date2),
	diff({{Y1,M1+12,D1},T1},NextDate2);

diff({{Y1,M1,D1},{H1,I1,S1}}, {{Y2,M2,D2},{H2,I2,S2}}) ->
	{{Y1-Y2, M1-M2, D1-D2}, {H1-H2, I1-I2, S1-S2}}.


	