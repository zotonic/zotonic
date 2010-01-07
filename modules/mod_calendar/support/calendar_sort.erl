%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-12-02
%% @doc Calendar support, sorts a list of events into buckets for display.

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

-module(calendar_sort).
-author("Marc Worrell <marc@worrell.nl>").

-export([
	sort/1,
	sort/2,
	test/0
]).

-include_lib("zotonic.hrl").
-include_lib("../include/mod_calendar.hrl").


%% @doc Sort the events into per-day buckets. Days are separated at the cut off time.
%% @spec sort([#event]) -> { [{date(),[#event]}], [{date(),[#event]}] }
sort(Events) ->
	sort(Events, 0).
sort(Events, DayBorderHour) ->
	DayBorder = {DayBorderHour, 0, 0},
	%% 1. Split events that overlap the day borders into an event per day.
	Split = lists:flatten([ split_event(E, DayBorder) || E <- Events ]),
	%% 2. Split the events in whole day and partial day events
	{WholeDay,PartDay} = lists:partition(fun is_wholeday/1, Split),
	%% 3. Sort all events into separate days.
	Days = day_buckets(lists:sort(PartDay)),
	WholePerDay = day_buckets(lists:sort(WholeDay)),
	%% 4. Sort every day by the duration of the events.
	DaysDuration = [ duration_bucket(Bucket, DayBorderHour) || Bucket <- Days ],
	%% 5. Place events per day, longest events first.
	{[ {Day, maxlevel(schedule(Evs))} || {Day,Evs} <- DaysDuration ], WholePerDay}.


%% @doc Assign the maximum level for each event, taking into account the nested overlapping events.
%% I assume that there are not too many events, so use a brute force method.
maxlevel(Events) ->
	%% Sort all events on their level.
	Sorted = lists:sort(fun compare_level/2, Events),
	%% Make a initial list or regions and their max level
	MaxLevels = fetch_ranges(Sorted, []),
	MaxLevels1 = combine_ranges(MaxLevels, []),
	[ Ev#calendar_event{max_level=find_overlap(Ev#calendar_event.sec_start, Ev#calendar_event.sec_end, MaxLevels1)}
	  || Ev <- Sorted ].
	
	compare_level(#calendar_event{level=A}, #calendar_event{level=B}) ->
		A > B.
		
	fetch_ranges([], MaxLevels) ->
		MaxLevels;
	fetch_ranges([#calendar_event{level=Lev, sec_start=From, sec_end=To}|Rest], MaxLevels) ->
		fetch_ranges(Rest, [{Lev, From, To}|MaxLevels]).

	%% Combine overlapping ranges into one range, assign max found level.
	%% Accumulator holds independent ranges that do not overlap.
	%% Repeat combining ranges until the range is stable, then proceed with next remaining range.
	combine_ranges([], Acc) ->
		Acc;
	combine_ranges([{Lev,From,To}|Rest], Acc) ->
		{Lev1,From1,To1,NoOverlap} = combine_overlap(Rest,Lev,From,To,[]),
		case {Lev1,From1,To1} of
			{Lev,From,To} -> combine_ranges(NoOverlap,[{Lev1,From,To}|Acc]);
			_ -> combine_ranges([{Lev1,From1,To1}|NoOverlap], Acc)
		end.

	combine_overlap([], Lev, From, To, NoOverlap) ->
		{Lev, From, To, NoOverlap};
	combine_overlap([{L,S,E}|Rest], Lev, From, To, NoOverlap) when To > S, From < E ->
		combine_overlap(Rest, max(L,Lev), min(From,S), max(To,E), NoOverlap);
	combine_overlap([R|Rest], Lev, From, To, NoOverlap) ->
		combine_overlap(Rest, Lev, From, To, [R|NoOverlap]).

	find_overlap(_From, _To, []) ->
		undefined;
	find_overlap(From, To, [{Lev,Start,End}|_]) when From < End, To > Start ->
		Lev;
	find_overlap(From, To, [_|Rest]) ->
		find_overlap(From, To, Rest).


%% @doc Schedule the events over the placeholder of the day. Every event gets an extra 'level' when they
%% overlap with another event on the same period. The maximum level over overlapping events is also calculated
%% so that the event can be displayed a bit narrower to make space for the overlapping events.
schedule(Events) ->
	{Events1, _MaxLevel, _OpenSpots} = lists:foldr(fun schedule1/2, {[],0,[]}, Events),
	Events1.
	
	schedule1(Event, {Placed,MaxLevel,Open}) ->
		{Level, MaxLevel1, Open1} = allocate(Event#calendar_event.sec_start, Event#calendar_event.sec_end, Open, MaxLevel, []),
		{[Event#calendar_event{level=Level}|Placed], MaxLevel1, Open1}.

	%% Find a spot in the list of open spots per level.
	allocate(Start, End, [], MaxLevel, Acc) ->
		allocate(Start, End, [{MaxLevel+1,0,86400}], MaxLevel+1, Acc);
	allocate(Start, End, [{Level,From,To}|Spots], MaxLevel, Acc) when Start >= From, End =< To ->
		Split = split_spot(Level, Start, End, From, To),
		{Level, MaxLevel, lists:reverse(Acc, Split++Spots)};
	allocate(Start, End, [{Level,_From,_To}=Spot|Spots], MaxLevel, Acc) ->
		allocate(Start, End, Spots, max(Level,MaxLevel), [Spot|Acc]).
	
	%% Split an open spot, remove the space from the start to the end. Return the list of remaining open spots.
	split_spot(_Level, Start, End, Start, End) ->
		[];
	split_spot(Level, Start, End, Start, To) ->
		[{Level, End, To}];
	split_spot(Level, Start, End, From, End) ->
		[{Level, From, Start}];
	split_spot(Level, Start, End, From, To) ->
		[{Level, From, Start}, {Level, End, To}].
	

%% @doc Normalize all time in a bucket relative to the day start, calculate the durations as well
duration_bucket({Day, Events}, DayBorderHour) ->
	{Day, lists:sort([ normalize_bucket_event(Day, Event, DayBorderHour) || Event <- Events ]) }.

	normalize_bucket_event(Day, Event, DayBorderHour) ->
		StartSecs = normalize_date(Day, Event#calendar_event.date_start, DayBorderHour),
		EndSecs = normalize_date(Day, Event#calendar_event.date_end, DayBorderHour),
		Event#calendar_event{duration=EndSecs-StartSecs, sec_start=StartSecs, sec_end=EndSecs}.

	%% @doc Normalize all times to seconds from the day start, called per bucket
	normalize_date(Date, {Date,{H,I,S}}, DayBorderHour) ->
		(H-DayBorderHour)*3600 + I*60 + S;
	normalize_date(_, {_,{H,I,S}}, DayBorderHour) ->
		(H-DayBorderHour+24)*3600 + I*60 + S.


%% Sort all events in separate buckets per calendar day.
day_buckets([]) ->
	[];
day_buckets([{Day,Event}|Rest]) ->
	day_buckets(Rest, Day, [Event], []).
	
	day_buckets([], Day, DayAcc, Acc) ->
		[{Day, DayAcc} | Acc];
	day_buckets([{Day,Event}|Rest], Day, DayAcc, Acc) ->
		day_buckets(Rest, Day, [Event|DayAcc], Acc);
	day_buckets([{NewDay, Event}|Rest], Day, DayAcc, Acc) ->
		day_buckets(Rest, NewDay, [Event], [{Day,DayAcc}|Acc]).


%% Split event into sub events, one per calendar day in the day range.
split_event(#calendar_event{date_start=Start, date_end=End} = Event, DayBorder) ->	
	StartDay = calday(Start, DayBorder),
	EndDay = calday(End, DayBorder),
	split_event(StartDay, EndDay, Event, DayBorder, []).

	split_event(StartDay, StartDay, Event, _DayBorder, Acc) ->
		[{StartDay,Event}|Acc];
	split_event(StartDay, EndDay, Event, DayBorder, Acc) ->
		{NextDay,_} = z_datetime:next_day({StartDay, {0,0,0}}),
		Event1 = Event#calendar_event{date_end={NextDay, DayBorder}},
		Event2 = Event#calendar_event{date_start={NextDay, DayBorder}},
		split_event(NextDay, EndDay, Event2, DayBorder, [{StartDay,Event1}|Acc]).


%% Return the day for the date in the calendar, using the dayborder time.
calday({_, Time} = DateTime, DayBorder) when Time < DayBorder ->
	{PrevDate,_} = z_datetime:prev_day(DateTime),
	PrevDate;
calday({Date, _Time}, _DayBorder) ->
	Date.


%% Check if an event is a whole day. This assumes the events are already placed in the day buckets
is_wholeday({_, #calendar_event{date_start={A,T},date_end={A,T}}}) when T =:= {0,0,0} ->
    true;
is_wholeday({_, #calendar_event{date_start={DateStart,Time}, date_end={DateEnd,Time}}}) when DateStart =/= DateEnd ->
    true;
is_wholeday(_) ->
    false.


max(A,B) when A > B -> A;
max(_,B) -> B.

min(A,B) when A > B -> B;
min(A,_) -> A.


%% @doc Unit tests for this module.
test() ->
	%% Split an event in three different days.
	Split = split_event(#calendar_event{date_start={{2009,6,1},{5,0,0}}, date_end={{2009,6,2},{10,0,0}}}, {6,0,0}),
	[{{2009,6,2},  #calendar_event{date_start={{2009,6,2},{6,0,0}}, date_end={{2009,6,2},{10,0,0}}}},
	 {{2009,6,1},  #calendar_event{date_start={{2009,6,1},{6,0,0}}, date_end={{2009,6,2},{ 6,0,0}}}},
	 {{2009,5,31}, #calendar_event{date_start={{2009,6,1},{5,0,0}}, date_end={{2009,6,1},{ 6,0,0}}}}] = Split,

	%% Test sort into buckets
	List = [
		{{2009,6,1}, a},
		{{2009,6,1}, b},
		{{2009,6,2}, c},
		{{2009,6,4}, d},
		{{2009,6,4}, e}
	],
	BucketList = day_buckets(lists:sort(List)),
	[{{2009,6,4},[e,d]},{{2009,6,2},[c]},{{2009,6,1},[b,a]}] = BucketList,
	
	%% Test the duration buckets.
	WithDurations = duration_bucket({
		{2009,5,31}, [
			#calendar_event{date_start={{2009,6,1 },{5,0,0}}, date_end={{2009,6,1},{ 6,0,0}}, id=a},
			#calendar_event{date_start={{2009,5,31},{6,0,0}}, date_end={{2009,6,1},{ 6,0,0}}, id=b},
			#calendar_event{date_start={{2009,5,31},{10,1,1}}, date_end={{2009,5,31},{11,2,2}}, id=c}
		]}, 6),
	{{2009,5,31},
		 [
			#calendar_event{duration=3600, sec_start=82800, sec_end=86400, 
							date_start={{2009,6,1},{5,0,0}}, date_end={{2009,6,1},{6,0,0}}, id=a},
			#calendar_event{duration=3661, sec_start=14461, sec_end=18122, 
							date_start={{2009,5,31},{10,1,1}}, date_end={{2009,5,31},{11,2,2}}, id=c},
			#calendar_event{duration=86400, sec_start=0, sec_end=86400, 
							date_start={{2009,5,31},{6,0,0}}, date_end={{2009,6,1},{6,0,0}}, id=b}
		]} = WithDurations,
	
	%% Test the scheduler, placing events at the right levels
	Evs = [
			#calendar_event{duration=3600, sec_start=82800, sec_end=86400, id=a},
			#calendar_event{duration=3661, sec_start=14461, sec_end=18122, id=c},
			#calendar_event{duration=86400, sec_start=0, sec_end=86400, id=b}
		],
	SchEvs = schedule(Evs),
	[
			#calendar_event{duration=3600, sec_start=82800, sec_end=86400, level=2, id=a},
			#calendar_event{duration=3661, sec_start=14461, sec_end=18122, level=2, id=c},
			#calendar_event{duration=86400, sec_start=0, sec_end=86400, level=1, id=b}
	] = SchEvs,

	%% Test the assignment of max levels
	LevEvs = [
		#calendar_event{duration=100, sec_start=0, sec_end=100, level=1, id=e},
		#calendar_event{duration=3600, sec_start=82800, sec_end=86400, level=3, id=d},
		#calendar_event{duration=3600, sec_start=82800, sec_end=86400, level=2, id=a},
		#calendar_event{duration=3661, sec_start=14461, sec_end=18122, level=2, id=c},
		#calendar_event{duration=86400, sec_start=100, sec_end=86400, level=1, id=b}
	],
	MaxLevEvs = maxlevel(LevEvs),
	[
			#calendar_event{duration=3600, sec_start=82800, sec_end=86400, level=3, max_level=3, id=d},
			#calendar_event{duration=3661, sec_start=14461, sec_end=18122, level=2, max_level=3, id=c},
			#calendar_event{duration=3600, sec_start=82800, sec_end=86400, level=2, max_level=3, id=a},
			#calendar_event{duration=86400, sec_start=100, sec_end=86400, level=1, max_level=3, id=b},
			#calendar_event{duration=100, sec_start=0, sec_end=100, level=1, max_level=1, id=e}
	] = MaxLevEvs,
	ok.
