%%%-------------------------------------------------------------------
%%% Adapted and expanded for Zotonic by Marc Worrell <marc@worrell.nl>
%%%-------------------------------------------------------------------

-module(erlydtl_dateformat).
-export([format/2, format/3]).

-include_lib("zotonic.hrl").

-define(TAG_SUPPORTED(C),
    C =:= $a orelse
    C =:= $A orelse
    C =:= $b orelse
    C =:= $B orelse
    C =:= $c orelse
    C =:= $d orelse
    C =:= $D orelse
    C =:= $f orelse
    C =:= $F orelse
    C =:= $g orelse
    C =:= $G orelse
    C =:= $h orelse
    C =:= $H orelse
    C =:= $i orelse
    C =:= $I orelse
    C =:= $j orelse
    C =:= $l orelse
    C =:= $L orelse
    C =:= $m orelse
    C =:= $M orelse
    C =:= $n orelse
    C =:= $N orelse
    C =:= $O orelse
    C =:= $P orelse
    C =:= $r orelse
    C =:= $s orelse
    C =:= $S orelse
    C =:= $t orelse
    C =:= $T orelse
    C =:= $U orelse
    C =:= $w orelse
    C =:= $W orelse
    C =:= $y orelse
    C =:= $Y orelse
    C =:= $z orelse
    C =:= $Z
).

%
% Format the current date/time
%
format(FormatString, #context{} = Context) ->
    {Date, Time} = erlang:localtime(),
    iolist_to_binary(replace_tags(Date, Time, FormatString, Context)).

%
% Format a tuple of the form {{Y,M,D},{H,M,S}}
% This is the format returned by erlang:localtime()
% and other standard date/time BIFs
%
format({{9999,_,_},_}, _FormatString, _Context) ->
	undefined;
format({{_,_,_} = Date,{_,_,_} = Time}, FormatString, Context) ->
   iolist_to_binary(replace_tags(Date, Time, FormatString, Context));

%
% Format a tuple of the form {Y,M,D}
%
format({_,_,_} = Date, FormatString, Context) ->
    iolist_to_binary(replace_tags(Date, {0,0,0}, FormatString, Context));
format(DateTime, FormatString, _Context) ->
   ?DEBUG(io_lib:format("Unrecognised date parameter : ~p~n", [DateTime])),
   FormatString.

replace_tags(Date, Time, Input, Context) when is_binary(Input) ->
    replace_tags(Date, Time, binary_to_list(Input), Context);
replace_tags(Date, Time, Input, Context) ->
    replace_tags(Date, Time, Input, [], noslash, Context).
replace_tags(_Date, _Time, [], Out, _State, _Context) ->
    lists:reverse(Out);
replace_tags(Date, Time, [C|Rest], Out, noslash, Context) when ?TAG_SUPPORTED(C) ->
    replace_tags(Date, Time, Rest, [tag_to_value(C, Date, Time, Context)|Out], noslash, Context);
replace_tags(Date, Time, [$\\|Rest], Out, noslash, Context) ->
    replace_tags(Date, Time, Rest, Out, slash, Context);
replace_tags(Date, Time, [C|Rest], Out, slash, Context) ->
    replace_tags(Date, Time, Rest, [C|Out], noslash, Context);
replace_tags(Date, Time, [C|Rest], Out, _State, Context) ->
    replace_tags(Date, Time, Rest, [C|Out], noslash, Context).


%-----------------------------------------------------------
% Time formatting
%-----------------------------------------------------------

% 'a.m.' or 'p.m.'
tag_to_value($a, _, {H, _, _}, _Context) when H > 11 -> "p.m.";
tag_to_value($a, _, _, _Context) -> "a.m.";

% 'AM' or 'PM'
tag_to_value($A, _, {H, _, _}, _Context) when H > 11 -> "PM";
tag_to_value($A, _, _, _Context) -> "AM";

% Swatch Internet time
tag_to_value($B, _, _, _Context) ->
   ""; % NotImplementedError

%
% Time, in 12-hour hours and minutes, with minutes
% left off if they're zero.
%
% Examples: '1', '1:30', '2:05', '2'
%
% Proprietary extension.
%
tag_to_value($f, Date, {H, 0, S}, Context) ->
   % If min is zero then return the hour only
   tag_to_value($g, Date, {H, 0, S}, Context);
tag_to_value($f, Date, Time, Context) ->
   % Otherwise return hours and mins
   tag_to_value($g, Date, Time, Context)
      ++ ":" ++ tag_to_value($i, Date, Time, Context);

% Hour, 12-hour format without leading zeros; i.e. '1' to '12'
tag_to_value($g, _, {H,_,_}, _Context) ->
   integer_to_list(hour_24to12(H));

% Hour, 24-hour format without leading zeros; i.e. '0' to '23'
tag_to_value($G, _, {H,_,_}, _Context) ->
   integer_to_list(H);

% Hour, 12-hour format; i.e. '01' to '12'
tag_to_value($h, _, {H,_,_}, _Context) ->
   integer_to_list_zerofill(hour_24to12(H));

% Hour, 24-hour format; i.e. '00' to '23'
tag_to_value($H, _, {H,_,_}, _Context) ->
   integer_to_list_zerofill(H);

% Minutes; i.e. '00' to '59'
tag_to_value($i, _, {_,M,_}, _Context) ->
   integer_to_list_zerofill(M);

% Time, in 12-hour hours, minutes and 'a.m.'/'p.m.', with minutes left off
% if they're zero and the strings 'midnight' and 'noon' if appropriate.
% Examples: '1 a.m.', '1:30 p.m.', 'midnight', 'noon', '12:30 p.m.'
% Proprietary extension.
tag_to_value($P, _, {0,  0, _}, Context) -> l10n_date:label(midnight, Context);
tag_to_value($P, _, {12, 0, _}, Context) -> l10n_date:label(noon, Context);
tag_to_value($P, Date, Time, Context) ->
   tag_to_value($f, Date, Time, Context)
      ++ " " ++ tag_to_value($a, Date, Time, Context);

% Seconds; i.e. '00' to '59'
tag_to_value($s, _, {_,_,S}, _Context) ->
   integer_to_list_zerofill(S);

%-----------------------------------------------------------
% Date formatting
%-----------------------------------------------------------

% Month, textual, 3 letters, lowercase; e.g. 'jan'
tag_to_value($b, {_,M,_}, _, Context) ->
   z_string:truncate(l10n_date:monthname(M, Context), 3, []);

% ISO 8601 date format - 2004-02-12T15:19:21+00:00
tag_to_value($c, Date, Time, Context) ->
    LTime = {Date, Time},
    UTime = erlang:localtime_to_universaltime(LTime),
    DiffSecs = calendar:datetime_to_gregorian_seconds(LTime) - 
       calendar:datetime_to_gregorian_seconds(UTime),
    {Secs, Sign} = case DiffSecs < 0 of
        true -> {0-DiffSecs, $-};
        false -> {DiffSecs, $+}
    end,    
    Hours   = Secs div 3600,
    Minutes = (Secs rem 3600) div 60,
    replace_tags(Date, Time, "Y-m-d", Context) 
        ++ [$T | replace_tags(Date, Time, "H:i:s", Context)]
        ++ [Sign|integer_to_list_zerofill(Hours)]
        ++ [$:|integer_to_list_zerofill(Minutes)];

% Day of the month, 2 digits with leading zeros; i.e. '01' to '31'
tag_to_value($d, {_, _, D}, _, _Context) ->
   integer_to_list_zerofill(D);

% Day of the week, textual, 3 letters; e.g. 'Fri'
tag_to_value($D, Date, _, Context) ->
   Dow = calendar:day_of_the_week(Date),
   z_string:truncate(l10n_date:dayname(Dow, Context), 3, []);

% Month, textual, long; e.g. 'January'
tag_to_value($F, {_,M,_}, _, Context) ->
   l10n_date:monthname(M, Context);

% '1' if Daylight Savings Time, '0' otherwise.
tag_to_value($I, _, _, _Context) ->
   "TODO";

% Day of the month without leading zeros; i.e. '1' to '31'
tag_to_value($j, {_, _, D}, _, _Context) ->
   integer_to_list(D);

% Day of the week, textual, long; e.g. 'Friday'
tag_to_value($l, Date, _, Context) ->
   l10n_date:dayname(calendar:day_of_the_week(Date), Context);

% Boolean for whether it is a leap year; i.e. True or False
tag_to_value($L, {Y,_,_}, _, _Context) ->
   case calendar:is_leap_year(Y) of
   true -> "True";
   _ -> "False"
   end;

% Month; i.e. '01' to '12'
tag_to_value($m, {_, M, _}, _, _Context) ->
   integer_to_list_zerofill(M);

% Month, textual, 3 letters; e.g. 'Jan'
tag_to_value($M, {_,M,_}, _, Context) ->
    case l10n_date:monthname_short(M, Context) of
        [] -> z_string:truncate(l10n_date:monthname(M, Context), 3, []);
        S -> S
    end;

% Month without leading zeros; i.e. '1' to '12'
tag_to_value($n, {_, M, _}, _, _Context) ->
   integer_to_list(M);

% Month abbreviation in Associated Press style. Proprietary extension.
tag_to_value($N, {_,M,_}, _, Context) when M =:= 9 ->
   % Special case - "Sept."
   z_string:truncate(l10n_date:monthname(M, Context), 4, ".");
tag_to_value($N, {_,M,_}, _, Context) when M < 3 orelse M > 7 ->
   % Jan, Feb, Aug, Oct, Nov, Dec are all
   % abbreviated with a full-stop appended.
   z_string:truncate(l10n_date:monthname(M, Context), 3, ".");
tag_to_value($N, {_,M,_}, _, Context) ->
   % The rest are the fullname.
   l10n_date:monthname(M, Context);

% Difference to Greenwich time in hours; e.g. '+0200'
tag_to_value($O, Date, Time, _Context) ->
   Diff = utc_diff(Date, Time),
   Offset = if
      Diff < 0 ->
          io_lib:format("-~4..0w", [abs(Diff)]);
      true ->
          io_lib:format("+~4..0w", [Diff])
   end,
   lists:flatten(Offset);

% RFC 2822 formatted date; e.g. 'Thu, 21 Dec 2000 16:01:07 +0200'
tag_to_value($r, Date, Time, Context) ->
   replace_tags(Date, Time, "D, j M Y H:i:s O", Context);

% English ordinal suffix for the day of the month, 2 characters;
% i.e. 'st', 'nd', 'rd' or 'th'
tag_to_value($S, {_, _, D}, _, _Context) when
   D rem 100 =:= 11 orelse
   D rem 100 =:= 12 orelse
   D rem 100 =:= 13 -> "th";
tag_to_value($S, {_, _, D}, _, _Context) when D rem 10 =:= 1 -> "st";
tag_to_value($S, {_, _, D}, _, _Context) when D rem 10 =:= 2 -> "nd";
tag_to_value($S, {_, _, D}, _, _Context) when D rem 10 =:= 3 -> "rd";
tag_to_value($S, _, _, _Context) -> "th";

% Number of days in the given month; i.e. '28' to '31'
tag_to_value($t, {Y,M,_}, _, _Context) ->
   integer_to_list(calendar:last_day_of_the_month(Y,M));

% Time zone of this machine; e.g. 'EST' or 'MDT'
tag_to_value($T, _, _, _Context) ->
   "TODO";

% Seconds since the Unix epoch (January 1 1970 00:00:00 GMT)
tag_to_value($U, Date, Time, _Context) ->
	[UtcTime|_] = calendar:local_time_to_universal_time_dst({Date, Time}), 
    EpochSecs = calendar:datetime_to_gregorian_seconds(UtcTime)
       			- 62167219200, % calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    integer_to_list(EpochSecs);

% Day of the week, numeric, i.e. '0' (Sunday) to '6' (Saturday)
tag_to_value($w, Date, _, _Context) ->
   % Note: calendar:day_of_the_week returns
   %   1 | .. | 7. Monday = 1, Tuesday = 2, ..., Sunday = 7
   integer_to_list(calendar:day_of_the_week(Date) rem 7);

% ISO-8601 week number of year, weeks starting on Monday
tag_to_value($W, {Y,M,D}, _, _Context) ->
   integer_to_list(year_weeknum(Y,M,D));

% Year, 2 digits; e.g. '99'
tag_to_value($y, {Y, _, _}, _, _Context) ->
   Y1 = Y rem 100,
   [ Y1 div 10 + $0, Y1 rem 10 + $0];

% Year, 4 digits; e.g. '1999'
tag_to_value($Y, {Y, _, _}, _, _Context) ->
   integer_to_list(Y);

% Day of the year; i.e. '0' to '365'
tag_to_value($z, {Y,M,D}, _, _Context) ->
    integer_to_list(day_of_year(Y,M,D));

% Time zone offset in seconds (i.e. '-43200' to '43200'). The offset for
% timezones west of UTC is always negative, and for those east of UTC is
% always positive.
tag_to_value($Z, _, _, _Context) ->
   "TODO";

tag_to_value(C, Date, Time, _Context) ->
    io:format("Unimplemented tag : ~p [Date : ~p] [Time : ~p]", [C, Date, Time]),
    "".

% Date helper functions
day_of_year(Y,M,D) ->
   day_of_year(Y,M,D,0).
day_of_year(_Y,M,D,Count) when M =< 1 ->
   D + Count;
day_of_year(Y,M,D,Count) when M =< 12 ->
   day_of_year(Y, M - 1, D, Count + calendar:last_day_of_the_month(Y,M));
day_of_year(Y,_M,D,_Count) ->
   day_of_year(Y, 12, D, 0).

hour_24to12(0) -> 12;
hour_24to12(H) when H < 13 -> H;
hour_24to12(H) when H < 24 -> H - 12;
hour_24to12(H) -> H.

year_weeknum(Y,M,D) -> 
    First = (calendar:day_of_the_week(Y, 1, 1) rem 7) - 1,
    Wk = ((((calendar:date_to_gregorian_days(Y, M, D) -
            calendar:date_to_gregorian_days(Y, 1, 1)) + First) div 7)
           + (case First < 4 of true -> 1; _ -> 0 end)),
    case Wk of
       0 -> weeks_in_year(Y - 1);
       _ -> case weeks_in_year(Y) of
              WksInThisYear when Wk > WksInThisYear -> 1;
              _ -> Wk
            end
    end.
   
weeks_in_year(Y) ->
    D1 = calendar:day_of_the_week(Y, 1, 1),
    D2 = calendar:day_of_the_week(Y, 12, 31),
    if (D1 =:= 4 orelse D2 =:= 4) -> 53; true -> 52 end.

utc_diff(Date, Time) ->
   LTime = {Date, Time},
   UTime = erlang:localtime_to_universaltime(LTime),
   DiffSecs = calendar:datetime_to_gregorian_seconds(LTime) - 
       calendar:datetime_to_gregorian_seconds(UTime),
   trunc((DiffSecs / 3600) * 100).


% Utility functions
integer_to_list_zerofill(N) when is_float(N) ->
    integer_to_list_zerofill(round(N));
integer_to_list_zerofill(N) when N < 100 ->
    [ N div 10 + $0, N rem 10 + $0];
integer_to_list_zerofill(N) ->
    integer_to_list(N).
